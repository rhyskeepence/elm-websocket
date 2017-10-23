module ExampleApp exposing (..)

import Model exposing (..)
import State exposing (..)
import View exposing (..)
import Api
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
    Api.listen model.host Api.decodeMessage Receive
