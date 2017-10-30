{-# LANGUAGE OverloadedStrings #-}

module Elm.WebSocket.Server
  ( withWebSocketBroadcaster
  , newBroadcaster
  , broadcast
  ) where

import Elm.WebSocket.Types

import Control.Concurrent
import Control.Concurrent.STM.TChan
       (newBroadcastTChanIO, readTChan, dupTChan, writeTChan)

import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Foldable (traverse_)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS

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
    forkIO $ do
      channel <- atomically $ dupTChan incomingBroadcasts
      forever $ do
        message <- atomically $ readTChan channel
        WS.sendTextData connection message
  return ()

withWebSocketBroadcaster :: FromJSON a => ToJSON b => Broadcaster -> WebSocketServer a b -> Wai.Application -> Wai.Application
withWebSocketBroadcaster connectedClients server =
  WS.websocketsOr WS.defaultConnectionOptions $ webSocketApp connectedClients server

newBroadcaster :: IO Broadcaster
newBroadcaster = newBroadcastTChanIO

broadcast :: ToJSON a => Broadcaster -> a -> IO ()
broadcast broadcaster message = atomically $ writeTChan broadcaster $ encode message