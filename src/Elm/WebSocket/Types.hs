module Elm.WebSocket.Types where

import           Control.Concurrent.STM.TChan (TChan)
import           Data.ByteString.Lazy

type Broadcaster = TChan ByteString

type WebSocketServer a b = a -> IO (Maybe b)
