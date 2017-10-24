module View exposing (..)

import Api exposing (..)
import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


view : Model -> Html Msg
view model =
    div [ style flexContainer ]
        [ leftSidebar model
        , rightContent model ]


leftSidebar : Model -> Html Msg
leftSidebar model =
    div [ style sidebar ]
        [ ul [] (List.map sidebarTask model.allTasks) ]


sidebarTask : Api.Task -> Html Msg
sidebarTask task =
    div []
        [ (text task.name)
        , (text task.description) ]


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