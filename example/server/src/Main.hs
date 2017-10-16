{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Broadcast  as Broadcast
import           Data.Aeson                    (FromJSON, ToJSON)
import           Elm.WebSocket                 as WS
import           GHC.Generics                  (Generic)
import           Network.HTTP.Types            (status200)
import           Network.Wai                   (Application, responseLBS)
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Middleware.Static
import           Web.Scotty

newtype Message = SparklineUpdate
  { newValue :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Message

instance FromJSON Message

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
