module JsonRender.State exposing (get, set, push, remove)

{-| JSON Pointer (RFC 6901) operations on JSON Values.
-}

import Array
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)


{-| Parse a JSON Pointer path into segments.
"" -> []
"/name" -> ["name"]
"/user/name" -> ["user", "name"]
-}
parsePath : String -> List String
parsePath path =
    if path == "" then
        []

    else
        path
            |> String.dropLeft 1
            |> String.split "/"


{-| Get a value at a JSON Pointer path.
-}
get : String -> Value -> Maybe Value
get path value =
    let
        segments =
            parsePath path
    in
    getBySegments segments value


getBySegments : List String -> Value -> Maybe Value
getBySegments segments value =
    case segments of
        [] ->
            Just value

        segment :: rest ->
            let
                -- Try as object field
                fieldResult =
                    Decode.decodeValue (Decode.field segment Decode.value) value

                -- Try as array index
                indexResult =
                    case String.toInt segment of
                        Just idx ->
                            Decode.decodeValue (Decode.index idx Decode.value) value

                        Nothing ->
                            Err (Decode.Failure "not an index" value)
            in
            case fieldResult of
                Ok child ->
                    getBySegments rest child

                Err _ ->
                    case indexResult of
                        Ok child ->
                            getBySegments rest child

                        Err _ ->
                            Nothing


{-| Set a value at a JSON Pointer path.
-}
set : String -> Value -> Value -> Value
set path newValue state =
    let
        segments =
            parsePath path
    in
    setBySegments segments newValue state


setBySegments : List String -> Value -> Value -> Value
setBySegments segments newValue state =
    case segments of
        [] ->
            newValue

        [ segment ] ->
            setField segment newValue state

        segment :: rest ->
            let
                child =
                    get ("/" ++ segment) state
                        |> Maybe.withDefault (Encode.object [])

                updatedChild =
                    setBySegments rest newValue child
            in
            setField segment updatedChild state


setField : String -> Value -> Value -> Value
setField key value obj =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) obj of
        Ok pairs ->
            let
                updated =
                    if List.any (\( k, _ ) -> k == key) pairs then
                        List.map
                            (\( k, v ) ->
                                if k == key then
                                    ( k, value )

                                else
                                    ( k, v )
                            )
                            pairs

                    else
                        pairs ++ [ ( key, value ) ]
            in
            Encode.object updated

        Err _ ->
            -- Try array index
            case String.toInt key of
                Just idx ->
                    case Decode.decodeValue (Decode.list Decode.value) obj of
                        Ok items ->
                            items
                                |> List.indexedMap
                                    (\i v ->
                                        if i == idx then
                                            value

                                        else
                                            v
                                    )
                                |> Encode.list identity

                        Err _ ->
                            obj

                Nothing ->
                    obj


{-| Append a value to an array at a JSON Pointer path.
-}
push : String -> Value -> Value -> Value
push path newValue state =
    case get path state of
        Just arrayValue ->
            case Decode.decodeValue (Decode.list Decode.value) arrayValue of
                Ok items ->
                    set path (Encode.list identity (items ++ [ newValue ])) state

                Err _ ->
                    state

        Nothing ->
            state


{-| Remove a value at a JSON Pointer path.
-}
remove : String -> Value -> Value
remove path state =
    let
        segments =
            parsePath path
    in
    case segments of
        [] ->
            Encode.null

        _ ->
            let
                parentPath =
                    segments
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse

                lastSegment =
                    segments
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault ""
            in
            let
                parentPointer =
                    if List.isEmpty parentPath then
                        ""

                    else
                        "/" ++ String.join "/" parentPath
            in
            case get parentPointer state of
                Just parentValue ->
                    let
                        updatedParent =
                            removeField lastSegment parentValue
                    in
                    if List.isEmpty parentPath then
                        updatedParent

                    else
                        set parentPointer updatedParent state

                Nothing ->
                    state


removeField : String -> Value -> Value
removeField key obj =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) obj of
        Ok pairs ->
            pairs
                |> List.filter (\( k, _ ) -> k /= key)
                |> Encode.object

        Err _ ->
            case String.toInt key of
                Just idx ->
                    case Decode.decodeValue (Decode.list Decode.value) obj of
                        Ok items ->
                            items
                                |> List.indexedMap Tuple.pair
                                |> List.filter (\( i, _ ) -> i /= idx)
                                |> List.map Tuple.second
                                |> Encode.list identity

                        Err _ ->
                            obj

                Nothing ->
                    obj
