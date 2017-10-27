module Model exposing (..)


import Api
import Navigation

type alias NewTaskForm =
    { taskName : String
    , taskDescription : String }

type Page
    = Initial
    | CreateTask NewTaskForm
    | ViewTask Api.Task

type alias Model =
    { host : String
    , allTasks : List Api.Task
    , page : Page }


type Msg
    = Receive (Result String Api.Message)
    | UrlChange Navigation.Location