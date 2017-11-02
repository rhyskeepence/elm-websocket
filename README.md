# Elm WebSocket

Generate an Elm Subscriber and JSON encoders/decoders for a Wai WebSocket server.

Elm encode/decode is generated thanks to [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export), with ADT generation thanks to (https://github.com/FPtje).

## Installation

Available on [Hackage](https://hackage.haskell.org/package/elm-websocket-1.0) as ```elm-websocket```

## Usage

This package broadly does two things:
 1. A library for creating a Wai WebSocket service, which can respond to requests as well as broadcast to all clients.
 2. Generates the Elm code for data types, JSON encoders/decoders and a WebSocket subscriber

First let's create our API - some haskell types to model our Request and Response. We derive Generic and ElmType,  
so that [Elm Export](https://github.com/krisajenkins/elm-export) can do it's thing. We also 
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

Next, create a server. In this example, `httpApplication` can be any IO Wai.Application, for example a [Scotty](https://hackage.haskell.org/package/scotty) or [Servant](https://hackage.haskell.org/package/servant) application.

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

That's it from the server side. We have built a server which can respond to WebSocket requests, 
as well as send broadcast messages to all clients. Note that the request, response and broadcast types
are our Haskell types - JSON encoding/decoding is done under the covers.

The next bit is the Elm side. This package includes a code generator for Elm types, JSON encoders/decoders, and WebSocket subscription.

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
    renderSubscriber (Proxy :: Proxy Request) (Proxy :: Proxy Response)

main :: IO ()
main = specsToDir [spec] "client/src"
```

Note that `renderSubscriber` takes the `Request` and `Response` type - this is so that the generated `listen` and `send` functions
expect the correct types, and the compiler not allow any other type to be sent or received.

Run this, and `client/src/Api.elm` will be created.

We can then subscribe to WebSocket responses, and trigger a Msg of type `Receive (Result String Response)`:

```elm
subscriptions : Sub Msg
subscriptions =
    Api.listen "hostname:port" Receive
```

We can also send WebSocket requests to the server, using this Cmd:

```elm
    Api.send "hostname:port" (Api.CreateTask "name" "description")
```

#### Elm Notes

 1. The listen and send functions require the hostname of the server. This can be taken from the browser location and stored in the model, by using the `elm-lang/navigation` package. 
 2. The generated Elm code requires the following packages:
   ```
   elm package install elm-lang/websocket
   elm package install NoRedInk/elm-decode-pipeline
   elm package install krisajenkins/elm-exts
   ```

## Application design notes

The example application makes all requests and responses over WebSockets. From a performance standpoint
this design can be good, as the connection remains open so HTTP negotiation is minimised. However, it requires that
clients of your API use WebSockets, which may not be ideal. It also makes it difficult to perform ad-hoc requests using cURL/Postman/etc,
and viewing network activity is not as simple compared to a traditional REST API.

It is possible to use this library to write a system that uses REST to make Requests, and still broadcast events in HTTP request handlers, i.e., `liftIO $ broadcast ...`. 
Furthermore, the Elm boilerplate can be reduced if the REST API uses the generated Request and Response types.

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
