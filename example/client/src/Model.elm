module Model exposing (..)


import Api
import Navigation


type alias Model =
    { host : String
    , allTasks : List Api.Task
    , hoveredTask : Maybe Api.Task
    , visibleTask : Maybe Api.Task }


type Msg
    = Receive (Result String Api.Message)
    | UrlChange Navigation.Location