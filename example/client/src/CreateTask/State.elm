module CreateTask.State exposing (..)

import CreateTask.Model exposing (..)
import Model exposing (..)
import Api


updateCreateTaskForm : CreateTaskMsg -> CreateTaskForm -> Model -> ( Model, Cmd Msg )
updateCreateTaskForm msg form model =
    case msg of
        Save ->
            ({ model | page = Initial}, Api.send model.location.host (Api.CreateTaskRequest form.name form.description))

        SetName name ->
            let newForm = { form | name = name }
            in ({ model | page = CreateTask newForm }, Cmd.none)

        SetDescription description ->
            let newForm = { form | description = description }
            in ({ model | page = CreateTask newForm }, Cmd.none)

        ResizeDescription height ->
            let newForm = { form | descriptionHeight = height }
            in ({ model | page = CreateTask newForm }, Cmd.none)
