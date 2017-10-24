module State exposing (..)

import Api
import Model exposing (..)
import Navigation


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initialModel location, initialCommand location )


initialModel : Navigation.Location -> Model
initialModel location = Model location.host []


initialCommand : Navigation.Location -> Cmd Msg
initialCommand location = Api.send location.host Api.encodeMessage Api.LoadAllTasksRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Receive (Ok (Api.LoadAllTasksResponse tasks)) ->
            ( { model | allTasks = tasks }, Cmd.none )

        Receive (Ok _) ->
            -- Remove this by splitting Request and Response types
            ( model, Cmd.none )

        Receive (Err error) ->
            let
                d = Debug.log "Unable to decode websockets message " error
            in
                ( model , Cmd.none )

        UrlChange location ->
            ( { model | host = location.host }, Cmd.none )

