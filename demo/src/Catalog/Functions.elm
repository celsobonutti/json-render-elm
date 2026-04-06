module Catalog.Functions exposing (Functions, functions, toFunctionDict)

import Dict exposing (Dict)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue(..))


type alias ShoutParams =
    { text : String
    }


type alias AddParams =
    { a : Float
    , b : Float
    }


type alias Functions =
    { shout : ShoutParams -> String
    , add : AddParams -> Float
    }


functions : Functions
functions =
    { shout = \params -> String.toUpper params.text
    , add = \params -> params.a + params.b
    }


toFunctionDict : Functions -> Dict String (Dict String ResolvedValue -> ResolvedValue)
toFunctionDict fns =
    Dict.fromList
        [ ( "shout"
          , \args ->
                let
                    result =
                        ResolvedValue.succeed ShoutParams
                            |> ResolvedValue.required "text" ResolvedValue.string
                in
                case result args of
                    Ok params ->
                        RString (fns.shout params)

                    Err err ->
                        RError ("shout: " ++ err)
          )
        , ( "add"
          , \args ->
                let
                    result =
                        ResolvedValue.succeed AddParams
                            |> ResolvedValue.required "a" ResolvedValue.float
                            |> ResolvedValue.required "b" ResolvedValue.float
                in
                case result args of
                    Ok params ->
                        RFloat (fns.add params)

                    Err err ->
                        RError ("add: " ++ err)
          )
        ]
