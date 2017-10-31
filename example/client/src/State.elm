module State exposing (..)

import Api
import Model exposing (..)
import Navigation
import Page.CreateTaskPage as CreateTaskPage


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initialModel location, initialCommand location )


initialModel : Navigation.Location -> Model
initialModel location = Model location [] Initial


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
            ( { model | location = location }, Cmd.none )

        ShowCreateTask ->
            ( { model | page = (CreateTask (CreateTaskPage.newModel model.location)) }, Cmd.none)

        ShowViewTask task ->
            ( { model | page = (ViewTask task) }, Cmd.none)

        CreateTaskMsg msg ->
            case model.page of
                CreateTask createTaskModel ->
                    let
                        ( newModel, newCmd ) = CreateTaskPage.update msg createTaskModel
                    in
                        ( { model | page = CreateTask newModel }, Cmd.map CreateTaskMsg newCmd )

                _ -> (model, Cmd.none)

