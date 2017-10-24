module View exposing (..)


import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


view : Model -> Html Msg
view model =
    div [ style flexContainer ]
        [ leftNav model
        , rightContent model ]


leftNav : Model -> Html Msg
leftNav model =
    div [ style sidebar ]
        [ ]

rightContent : Model -> Html Msg
rightContent model =
    div [ style content ]
        [ ]


flexContainer : List (String, String)
flexContainer =
    [ ("display", "flex")
    , ("width", "100%")
    ]

sidebar : List (String, String)
sidebar =
    [ ("width", "20rem")
    , ("height", "100%")
    , ("color", "#ffffff")
    , ("background-color", "#546e7a")
    , ("border-right", "1px solid #29434e")
    ]

content : List (String, String)
content =
    [ ("flex", "1")
    , ("height", "100%")
    , ("background-color", "#b0bec5")
    ]