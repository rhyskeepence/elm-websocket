module Elm.WebSocket.Server
  ( withWebSocketBroadcaster
  , newBroadcaster
  , broadcast
  ) where

import           Elm.WebSocket.Types

import           Control.Concurrent
import qualified Control.Concurrent.Broadcast   as Broadcast (new, listen, broadcast)
import Control.Monad                 (forever)
import           Data.Aeson                     (ToJSON, encode)

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS


webSocketApp :: Broadcaster -> WS.ServerApp
webSocketApp incomingBroadcasts pendingConnection = do
  connection <- WS.acceptRequest pendingConnection
  WS.forkPingThread connection 30
  forkBroadcastThread connection incomingBroadcasts

  let loop = do
      WS.Text message <- WS.receiveDataMessage connection
      print message
      loop
  loop


forkBroadcastThread :: WS.Connection -> Broadcaster -> IO ()
forkBroadcastThread connection incomingBroadcasts = do
  _ <- forkIO $ forever $ do
    message <- Broadcast.listen incomingBroadcasts
    WS.sendTextData connection message
  return ()


withWebSocketBroadcaster :: Broadcaster -> Wai.Application -> Wai.Application
withWebSocketBroadcaster connectedClients =
  WS.websocketsOr WS.defaultConnectionOptions $ webSocketApp connectedClients


newBroadcaster :: IO Broadcaster
newBroadcaster =
  Broadcast.new


broadcast :: ToJSON a => Broadcaster -> a -> IO ()
broadcast broadcaster message =
  Broadcast.broadcast broadcaster $ encode message
