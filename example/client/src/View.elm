module View exposing (..)

import Api exposing (..)
import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Styles

view : Model -> Html Msg
view model =
    div [ style Styles.flexContainer ]
        [ leftSidebar model
        , rightContent model ]


leftSidebar : Model -> Html Msg
leftSidebar model =
    div [ style Styles.sidebar ]
        [ ul [] (sidebarMenu :: (List.map sidebarTask model.allTasks)) ]


sidebarMenu : Html Msg
sidebarMenu =
    li [ style Styles.sidebarTask ]
       [ text "+" ]

sidebarTask : Api.Task -> Html Msg
sidebarTask task =
    li [ style Styles.sidebarTask ]
       [ div [ style Styles.taskId ] [ (text (toString task.id)) ]
       , div [ style Styles.taskName ] [ (text task.name) ]
       , div [ style Styles.taskStatus ] [ (text (displayStatus task.status)) ] ]


displayStatus : Api.TaskStatus -> String
displayStatus status =
    case status of
        Ready -> "Ready"
        InPlay -> "In Play"
        Done -> "Done"

rightContent : Model -> Html Msg
rightContent model =
    div [ style Styles.content ]
        [ ]
