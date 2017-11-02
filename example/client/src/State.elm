module State exposing (..)

import Api exposing (..)
import Model exposing (..)
import Navigation
import CreateTask.Model exposing (resetCreateTaskForm)
import CreateTask.State exposing (updateCreateTaskForm)


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initialModel location, initialCommand location )


initialModel : Navigation.Location -> Model
initialModel location = Model location [] Initial


initialCommand : Navigation.Location -> Cmd Msg
initialCommand location = Api.send location.host Api.encodeRequest Api.LoadAllTasksRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Receive (Ok response) ->
            ( { model | allTasks = response.tasks }, Cmd.none )

        Receive (Err error) ->
            let
                d = Debug.log "Unable to decode websockets message " error
            in
                ( model , Cmd.none )

        UrlChange location ->
            ( { model | location = location }, Cmd.none )

        ShowCreateTask ->
            ( { model | page = (CreateTask resetCreateTaskForm) }, Cmd.none)

        ShowViewTask task ->
            ( { model | page = (ViewTask task) }, Cmd.none)

        CreateTaskMsg createTaskMsg ->
            case model.page of
                CreateTask form -> updateCreateTaskForm createTaskMsg form model
                _ -> (model, Cmd.none)

