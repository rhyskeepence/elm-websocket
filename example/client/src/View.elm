module View exposing (..)


import Api exposing (..)
import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import CreateTask.View exposing (viewCreateTask)
import ViewTask.View exposing (viewTask, displayStatus)

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ leftSidebar model
        , rightContent model ]


leftSidebar : Model -> Html Msg
leftSidebar model =
    div [ class "sidebar" ]
        [ ul [] (sidebarMenu :: (List.map sidebarTask model.allTasks)) ]


sidebarMenu : Html Msg
sidebarMenu =
    li [ class "sidebar-menu" ]
       [ button
             [ class "add-button"
             , onClick ShowCreateTask
             ]
             [ text "+ Add" ] ]


sidebarTask : Api.Task -> Html Msg
sidebarTask task =
    li [ class "sidebar-task"
       , onClick (ShowViewTask task) ]
       [ div [ class "task-id" ] [ (text (toString task.id)) ]
       , div [ class "task-name" ] [ (text task.name) ]
       , div [ class "task-status" ] [ (text (displayStatus task.status)) ] ]

rightContent : Model -> Html Msg
rightContent model =
    let
        body = case model.page of
            Initial -> []
            CreateTask form -> [ viewCreateTask form ]
            ViewTask task -> [ viewTask task ]
    in
        div [ class "content" ] body
