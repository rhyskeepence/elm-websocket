module Elm.WebSocket.Types where

import           Control.Concurrent
import qualified Network.WebSockets as WS

type ClientId = Int

type Client = (ClientId, WS.Connection)

type ConnectedClients = [Client]

type ConnectedClientsState = MVar [Client]
