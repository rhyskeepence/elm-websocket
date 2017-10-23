module Model exposing (..)


import Navigation

type alias Model =
    { host : String }


type Msg
    = UrlChange Navigation.Location