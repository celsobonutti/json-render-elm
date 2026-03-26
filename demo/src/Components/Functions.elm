module Components.Functions exposing (Functions, functions, toFunctionDict)

import Dict exposing (Dict)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue(..))


type alias ShoutParams =
    { text : String
    }


type alias Functions =
    { shout : ShoutParams -> String
    }


functions : Functions
functions =
    { shout = \params -> String.toUpper params.text
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
        ]
