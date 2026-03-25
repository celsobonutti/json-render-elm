module Components.Actions exposing (Action(..), decodeAction)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode exposing (Value)


type alias ExportParams =
    { format : String
    }


type Action
    = Export ExportParams
    | Press


decodeAction : String -> Dict String Value -> Result String Action
decodeAction name params =
    case name of
        "export" ->
            case Dict.get "format" params of
                Just format_raw ->
                    case Decode.decodeValue Decode.string format_raw of
                        Ok format ->
                            Ok (Export { format = format })

                        Err _ ->
                            Err "format must be a String"

                Nothing ->
                    Err "missing required param format"

        "press" ->
            Ok Press

        _ ->
            Err ("Unknown action: " ++ name)
