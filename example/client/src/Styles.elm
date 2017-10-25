module Styles exposing (..)

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

sidebarTask : List (String, String)
sidebarTask =
    [ ("padding", "2rem 1rem")
    , ("display", "flex")
    , ("border-bottom", "1px solid #29434e") ]

taskId : List (String, String)
taskId =
    [ ("margin-right", "1rem") ]

taskName : List (String, String)
taskName =
    [ ("font-weight", "600")
    , ("flex", "1") ]

taskStatus : List (String, String)
taskStatus =
    tag


tag : List (String, String)
tag =
    [ ("border-radius", "1rem")
    , ("background", "rgba(255, 255, 255, .3)")
    , ("padding", "0 5px") ]


content : List (String, String)
content =
    [ ("flex", "1")
    , ("height", "100%")
    , ("background-color", "#b0bec5")
    ]