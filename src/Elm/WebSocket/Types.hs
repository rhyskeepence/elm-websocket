module Elm.WebSocket.Types where

import           Control.Concurrent.Broadcast   (Broadcast)
import           Data.ByteString.Lazy

type Broadcaster = Broadcast ByteString

type WebSocketServer a b = a -> IO (Maybe b)
