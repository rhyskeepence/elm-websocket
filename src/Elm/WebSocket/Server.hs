module Elm.WebSocket.Server
  ( withWebSocketBroadcaster
  , newBroadcaster
  , broadcast
  ) where

import           Elm.WebSocket.Types

import           Control.Concurrent
import qualified Control.Concurrent.Broadcast   as Broadcast (broadcast, listen,
                                                              new)
import           Control.Monad                  (forever)
import           Data.Aeson                     (FromJSON, ToJSON, encode, decode)
import           Data.Foldable                  (traverse_)

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

webSocketApp :: FromJSON a => ToJSON b => Broadcaster -> WebSocketServer a b -> WS.ServerApp
webSocketApp incomingBroadcasts server pendingConnection = do
  connection <- WS.acceptRequest pendingConnection
  WS.forkPingThread connection 30
  forkBroadcastThread connection incomingBroadcasts

  let loop = do
        WS.Text message <- WS.receiveDataMessage connection
        traverse_ (handleRequest connection server) (decode message)
        loop
  loop


handleRequest :: FromJSON a => ToJSON b => WS.Connection -> WebSocketServer a b -> a -> IO ()
handleRequest connection server request = do
  response <- server request
  traverse_ (WS.sendTextData connection . encode) response
  return ()


forkBroadcastThread :: WS.Connection -> Broadcaster -> IO ()
forkBroadcastThread connection incomingBroadcasts = do
  _ <-
    forkIO $
    forever $
    do message <- Broadcast.listen incomingBroadcasts
       WS.sendTextData connection message
  return ()


withWebSocketBroadcaster :: FromJSON a => ToJSON b => Broadcaster -> WebSocketServer a b -> Wai.Application -> Wai.Application
withWebSocketBroadcaster connectedClients server = WS.websocketsOr WS.defaultConnectionOptions $ webSocketApp connectedClients server


newBroadcaster :: IO Broadcaster
newBroadcaster = Broadcast.new


broadcast :: ToJSON a => Broadcaster -> a -> IO ()
broadcast broadcaster message = Broadcast.broadcast broadcaster $ encode message
