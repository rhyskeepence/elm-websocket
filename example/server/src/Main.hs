{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import           Control.Concurrent.Broadcast  as Broadcast
import           Elm.WebSocket                 as WS
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Middleware.Static
import           Web.Scotty

main :: IO ()
main = do
  broadcaster <- Broadcast.new
  httpApplication <- scottyApp httpEndpoints
  Warp.run 8080 $ WS.withWebSocketBroadcaster broadcaster webSocketService httpApplication

webSocketService :: WebSocketServer Message Message
webSocketService _ = return Nothing

httpEndpoints :: ScottyM ()
httpEndpoints = do
  middleware $ staticPolicy (noDots >-> addBase "assets")
  matchAny "/" $ redirect "/index.html"
