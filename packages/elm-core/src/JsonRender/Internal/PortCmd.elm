module JsonRender.Internal.PortCmd exposing (PortCmd, encode, portCmd)

import Json.Encode as Encode exposing (Value)


type alias PortCmd =
    { port_ : String
    , value : Value
    }


portCmd : String -> Value -> PortCmd
portCmd name val =
    { port_ = name, value = val }


encode : String -> PortCmd -> Value
encode instanceId cmd =
    Encode.object
        [ ( "instanceId", Encode.string instanceId )
        , ( "port", Encode.string cmd.port_ )
        , ( "value", cmd.value )
        ]
