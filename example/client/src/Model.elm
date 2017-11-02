module Model exposing (..)


import Api
import Navigation
import CreateTask.Model as CreateTaskModel


type Page
    = Initial
    | CreateTask CreateTaskModel.CreateTaskForm
    | ViewTask Api.Task


type alias Model =
    { location : Navigation.Location
    , allTasks : List Api.Task
    , page : Page }


type Msg
    = Receive (Result String Api.Response)
    | UrlChange Navigation.Location
    | ShowCreateTask
    | ShowViewTask Api.Task
    | CreateTaskMsg CreateTaskModel.CreateTaskMsg
