module ViewTask.View exposing (..)

import Api exposing (..)
import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, defaultValue, disabled, href, id, placeholder, rows, style, type_, value)
import Html.Events exposing (onInput, on, onSubmit)

viewTask : Task -> Html Msg
viewTask task =
    div [ class "viewer" ]
        [ header
            []
            [ div [ class "task-name" ] [text task.name]
            , div [ class "task-status" ] [ text (displayStatus task.status) ]
            ]
        , div
            [ class "task-description" ]
            [ text task.description ]
        ]


displayStatus : Api.TaskStatus -> String
displayStatus status =
    case status of
        Ready -> "Ready"
        InPlay -> "In Play"
        Done -> "Done"
