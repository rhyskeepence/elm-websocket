module Elm.WebSocket.Types where

import           Control.Concurrent.STM.TChan (TChan)
import           Data.ByteString.Lazy

{-|
  Broadcaster to send a message to all clients connected to the WebSocketServer.
-}
type Broadcaster = TChan ByteString

{-|
  A WebSocketServer handles incoming WebSocket requests of type a, and
  may choose to respond with a response of type b.
-}
type WebSocketServer a b = a -> IO (Maybe b)
