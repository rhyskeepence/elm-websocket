module View exposing (..)


import Api exposing (..)
import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Page.CreateTaskPage as CreateTaskPage exposing (view)

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
             , onClick ViewCreateTask
             ]
             [ text "+ Add" ] ]


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
    let
        body = case model.page of
            Initial -> []
            CreateTask createTaskModel -> [ CreateTaskPage.view createTaskModel |> Html.map CreateTaskMsg ]
            ViewTask _ -> []
    in
        div [ class "content" ] body
