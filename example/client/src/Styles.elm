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

sidebarMenu : List (String, String)
sidebarMenu =
    [ ("padding", "2rem 1rem")
    , ("display", "flex")
    , ("justify-content", "center")
    , ("align-items", "center")
    , ("border-bottom", "1px solid #29434e") ]

addButton : List (String, String)
addButton =
    [ ("border-radius", "100%")
    , ("width", "2rem")
    , ("height", "2rem")
    , ("display", "flex")
    , ("justify-content", "center")
    , ("align-items", "center")
    , ("background-color", "#ffffff")
    , ("color", "#546e7a")
    , ("font-weight", "900")
    , ("cursor", "pointer")
    ]

taskId : List (String, String)
taskId =
    [ ("margin-right", "1rem") ]

taskName : List (String, String)
taskName =
    [ ("flex", "1") ]

taskStatus : List (String, String)
taskStatus =
    tag


tag : List (String, String)
tag =
    [ ("border-radius", "1rem")
    , ("border", "1px solid rgba(255, 255, 255, .5)")
    , ("font-weight", "100")
    , ("padding", "0 5px") ]


content : List (String, String)
content =
    [ ("flex", "1")
    , ("height", "100%")
    , ("background-color", "#b0bec5")
    ]