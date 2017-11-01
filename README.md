# Elm WebSocket

Generate an Elm Subscriber and JSON encoders/decoders for a Wai WebSocket server.

Elm encode/decode is generated thanks to [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export), with ADT generation thanks to (https://github.com/FPtje).

## Installation

Available on [Hackage](https://hackage.haskell.org/package/elm-websocket-1.0) as ```elm-websocket```

## Usage

This package broadly does two things:
 1. A library for creating a WAI WebSocket service, which can respond to requests as well as broadcast to all clients.
 1. Generates the ELM code for data types, JSON codecs and a WebSocket subscriber

First let's create our API - some haskell types to model our Request and Response. We derive Generic and ElmType,  
so that (Elm Export)[https://github.com/krisajenkins/elm-export] can do it's thing. We also 
need a FromJSON instance for the Request, and ToJSON instance for the Response.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Elm.Export
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

module Api where

data Request
  = CreateTask Text Text
  | LoadAllTasks
  deriving (Eq, Show, Generic, ElmType)

instance FromJSON Request

data Response
  = AllTasksResponse [Task]
  | TaskCountUpdate Int
  deriving (Eq, Show, Generic, ElmType)

instance ToJSON Response
```

Next, let's write a function to handle WebSocket requests.

Let's also add a `Broadcaster`, so that when a task is created, we broadcast a `TaskCountUpdate` to all connected clients:

```haskell
import Elm.WebSocket

webSocketService :: Broadcaster -> WebSocketServer Request Response
webSocketService broadcaster request =
  case request of
    CreateTask name description -> do
      -- do some IO to create the task
      taskCount <- -- get the new taskCount 
      broadcast broadcaster $ TaskCountUpdate taskCount 
      return Nothing
    LoadAllTasks -> do      
      tasks <- -- do some IO to fetch all tasks      
      return $ Just $ AllTasksResponse tasks
```

Next, create a server. In this example, `httpApplication` can be any IO Wai.Application.

```haskell
import Api 
import Elm.WebSocket
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  httpApplication <- ...
  broadcaster <- newBroadcaster
  let webSocketApp = webSocketService broadcaster
  run 8080 $ withWebSocketBroadcaster broadcaster webSocketApp httpApplication     
```

That's it from the server side - a server which can respond to WebSocket requests from 
the client, as well as send broadcasts to all clients. Note that the request, response and broadcast types
are our Haskell types - JSON marshalling is done under the covers.

The next bit is the Elm side. This package includes a code generator for Elm types, JSON codecs, and WebSocket subscription.

Create a main to generate the Elm source from our Haskell API:

```haskell
module Main where

import Api
import Data.Proxy
import Elm.Export

spec :: Spec
spec =
  moduleSpec ["Api"] $ do
    renderType (Proxy :: Proxy Request)
    renderType (Proxy :: Proxy Response)
    renderEncoder (Proxy :: Proxy Request)
    renderDecoder (Proxy :: Proxy Response)
    renderSubscriber

main :: IO ()
main = specsToDir [spec] "client/src"
```

Run this, and `client/src/Api.elm` will be created.

We can then subscribe to WebSocket responses, and trigger a Msg of type `Receive (Result String Api.Response)`:

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Api.listen model.location.host Api.decodeResponse Receive
```

We can also send WebSocket requests to the server, using this Cmd:

```elm
    Api.send model.location.host Api.encodeRequest (Api.CreateTask "name" "description")
```

#### Elm Notes

 1. The above example assumes that the generated decoder is named `decodeResponse`, and the generated encoder is named `encodeResponse` - these
names are automatically derived from the Haskell Resquest/Response type. 
 2. The listen and send functions require the hostname of the server. This can be taken from the browser location and stored in the model, by using the `elm-lang/navigation` package. 
 3. The generated Elm code requires the following packages:
   ```
   elm package install elm-lang/websocket
   elm package install NoRedInk/elm-decode-pipeline
   elm package install krisajenkins/elm-exts
   ```

## Example application

An example 'Task Management' application is included in this repository under the example directory. It can be built and run using the following command

```
$ make run-example
```

This will build the haskell application, as well as the Elm client, and serve it at [localhost:8080](http://localhost:8080).


## Development

```
$ git clone https://github.com/rhyskeepence/elm-websocket.git
$ cd elm-websocket
$ make test
```
