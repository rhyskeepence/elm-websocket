module View exposing (..)

import Api exposing (..)
import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

view : Model -> Html Msg
view model =
    div [ class "flex-container" ]
        [ leftSidebar model
        , rightContent model ]


leftSidebar : Model -> Html Msg
leftSidebar model =
    div [ class "sidebar" ]
        [ ul [] (sidebarMenu :: (List.map sidebarTask model.allTasks)) ]


sidebarMenu : Html Msg
sidebarMenu =
    li [ class "sidebar-menu" ]
       [ button [ class "add-button" ] [ text "+ Add" ] ]


sidebarTask : Api.Task -> Html Msg
sidebarTask task =
    li [ class "sidebar-task" ]
       [ div [ class "task-id" ] [ (text (toString task.id)) ]
       , div [ class "task-name" ] [ (text task.name) ]
       , div [ class "task-status" ] [ (text (displayStatus task.status)) ] ]


displayStatus : Api.TaskStatus -> String
displayStatus status =
    case status of
        Ready -> "Ready"
        InPlay -> "In Play"
        Done -> "Done"

rightContent : Model -> Html Msg
rightContent model =
    div [ class "content" ]
        [ ]
