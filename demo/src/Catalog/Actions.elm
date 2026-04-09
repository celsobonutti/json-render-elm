module Catalog.Actions exposing (Action(..), actionConfig, decodeAction, handleAction)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import JsonRender.Actions as Actions


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


handleAction : Action -> Actions.Model Action -> ( Actions.Model Action, Cmd (Actions.Msg Action) )
handleAction _ model =
    ( model, Cmd.none )


actionConfig : Actions.ActionConfig Action
actionConfig =
    { handleAction = handleAction
    , decodeAction = decodeAction
    }
