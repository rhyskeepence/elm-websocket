module CreateTask.Model exposing (..)


type alias CreateTaskForm =
    { errors : List String
    , name : String
    , description : String
    , descriptionHeight : Int
    }


type CreateTaskMsg
    = Save
    | SetName String
    | SetDescription String
    | ResizeDescription Int


baseHeight : Int
baseHeight = 32


resetCreateTaskForm : CreateTaskForm
resetCreateTaskForm = CreateTaskForm [] "" "" baseHeight
