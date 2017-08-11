module Elm.WebSocket.Server
  ( withWebSocketBroadcaster
  , newConnectedClientsState
  , broadcast
  ) where

import           Elm.WebSocket.Types

import           Control.Concurrent
import qualified Control.Exception              as E
import qualified Control.Monad                  as Monad
import           Data.Aeson                     (ToJSON, encode)
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe

connectClient :: WS.Connection -> ConnectedClientsState -> IO ClientId
connectClient connection clientsRef =
  modifyMVar clientsRef $ \clients -> do
    let clientId = nextId clients
    return ((clientId, connection) : clients, clientId)

disconnectClient :: ClientId -> ConnectedClientsState -> IO ()
disconnectClient clientId clientsRef = modifyMVar_ clientsRef $ \clients -> return $ withoutClient clientId clients

nextId :: ConnectedClients -> ClientId
nextId = Maybe.maybe 0 (1 +) . Safe.maximumMay . List.map fst

withoutClient :: ClientId -> ConnectedClients -> ConnectedClients
withoutClient clientId = List.filter ((/=) clientId . fst)

listen_ :: WS.Connection -> IO ()
listen_ connection = do
  _ <- listen connection :: IO Text.Text
  return ()

listen :: WS.Connection -> IO a
listen connection = Monad.forever $ WS.receiveDataMessage connection

wsApp :: ConnectedClientsState -> WS.ServerApp
wsApp connectedClients pendingConnection = do
  connection <- WS.acceptRequest pendingConnection
  clientId <- connectClient connection connectedClients
  WS.forkPingThread connection 30
  E.finally (listen_ connection) (disconnectClient clientId connectedClients)

withWebSocketBroadcaster :: ConnectedClientsState -> Wai.Application -> Wai.Application
withWebSocketBroadcaster connectedClients = WS.websocketsOr WS.defaultConnectionOptions $ wsApp connectedClients

newConnectedClientsState :: IO ConnectedClientsState
newConnectedClientsState = newMVar []

broadcast :: ToJSON a => ConnectedClientsState -> a -> IO ()
broadcast connectedClients message = do
  clients <- readMVar connectedClients
  Monad.forM_ clients $ \(_, connection) -> WS.sendTextData connection $ encode message
