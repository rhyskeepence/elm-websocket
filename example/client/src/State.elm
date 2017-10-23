module State exposing (..)


import Model exposing (..)
import Navigation


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initialModel location, initialCommand )


initialModel : Navigation.Location -> Model
initialModel location = Model location.host

initialCommand : Cmd Msg
initialCommand = Cmd.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( { model | host = location.host }, Cmd.none )

