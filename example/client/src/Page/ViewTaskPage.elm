module Page.ViewTaskPage exposing (..)

import Api exposing (..)
import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, defaultValue, disabled, href, id, placeholder, rows, style, type_, value)
import Html.Events exposing (onInput, on, onSubmit)

view : Task -> Html Msg
view task =
    div []
        [ div
            []
            [ text task.name ]
        , div
            []
            [ text (displayStatus task.status) ]
        , div
            []
            [ text task.description ]
        ]


displayStatus : Api.TaskStatus -> String
displayStatus status =
    case status of
        Ready -> "Ready"
        InPlay -> "In Play"
        Done -> "Done"