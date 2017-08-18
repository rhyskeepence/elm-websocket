module Elm.WebSocket.Types where

import           Control.Concurrent.Broadcast   (Broadcast)
import           Data.ByteString.Lazy

type Broadcaster = Broadcast ByteString
