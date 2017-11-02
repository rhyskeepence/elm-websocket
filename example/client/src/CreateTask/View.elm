module CreateTask.View exposing (viewCreateTask)


import Html exposing (..)
import Html.Attributes exposing (attribute, class, defaultValue, disabled, href, id, placeholder, rows, style, type_, value)
import Html.Events exposing (onInput, on, onSubmit)
import Json.Decode
import Navigation
import Api
import Model as Model
import CreateTask.Model exposing (..)


viewCreateTask : CreateTaskForm -> Html Model.Msg
viewCreateTask model =
    div [ class "editor" ]
        [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
            [ ul [] (List.map viewError model.errors)
            , viewForm model
            ]
        ]


viewError : String -> Html Model.Msg
viewError error =
    li [ class "error-message" ] [ text error ]


viewForm : CreateTaskForm -> Html Model.Msg
viewForm model =
    Html.form
        [ onSubmit (Model.CreateTaskMsg Save) ]
        [ fieldset []
            [ input
                [ placeholder "Task Name"
                , onInput (Model.CreateTaskMsg << SetName)
                , value model.name
                ]
                []
            , textarea
                [ placeholder "What's this task about?"
                , style [("height", ((toString (model.descriptionHeight + 4)) ++ "px"))]
                , onInput (Model.CreateTaskMsg << SetDescription)
                , onKeyUp (Model.CreateTaskMsg << ResizeDescription)
                , value model.description
                ]
                []
            , button [ class "save-button" ]
                [ text "Create Task" ]
            ]
        ]


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
  Html.Events.on "keyup" (Json.Decode.map tagger onScrollJsonParser)


onScrollJsonParser : Json.Decode.Decoder Int
onScrollJsonParser =
    Json.Decode.at ["target", "scrollHeight"] Json.Decode.int
