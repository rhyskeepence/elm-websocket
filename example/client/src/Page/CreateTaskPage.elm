module Page.CreateTaskPage exposing (..)


import Html exposing (..)
import Html.Attributes exposing (attribute, class, defaultValue, disabled, href, id, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Navigation
import Api


type alias Model =
    { location : Navigation.Location
    , errors : List String
    , name : String
    , description : String
    }


newModel : Navigation.Location -> Model
newModel navigation = Model navigation [] "" ""


type Msg
    = Save
    | SetName String
    | SetDescription String


view : Model -> Html Msg
view model =
    div [ class "editor" ]
        [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
            [ ul [] (List.map viewError model.errors)
            , viewForm model
            ]
        ]


viewError : String -> Html Msg
viewError error =
    li [ class "error-message" ] [ text error ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form
        [ onSubmit Save ]
        [ fieldset []
            [ input
                [ placeholder "Task Name"
                , onInput SetName
                , defaultValue model.name
                ]
                []
            , textarea
                [ placeholder "What's this task about?"
                , onInput SetDescription
                , defaultValue model.description
                ]
                []
            , button [ class "save-button" ]
                [ text "Create Task" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Save ->
            (model, Api.send model.location.host Api.encodeMessage (Api.CreateTaskRequest model.name model.description))

        SetName name ->
            ({ model | name = name }, Cmd.none)

        SetDescription description ->
            ({ model | description = description }, Cmd.none)
