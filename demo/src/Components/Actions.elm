module Components.Actions exposing (Action(..))


type alias ExportParams =
    { format : String
    }


type Action
    = Export ExportParams
    | Press
