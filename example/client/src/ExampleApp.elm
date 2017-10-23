module ExampleApp exposing (..)

import Model exposing (..)
import State exposing (..)
import View exposing (..)
import Navigation

main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
