{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Api
import           Control.Concurrent.MVar       as MVar
import           Elm.WebSocket                 as WS
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Middleware.Static
import           Web.Scotty

main :: IO ()
main = do
  tasks <- MVar.newMVar []
  broadcaster <- WS.newBroadcaster
  httpApplication <- scottyApp httpEndpoints
  Warp.run 8080 $ WS.withWebSocketBroadcaster broadcaster (webSocketService tasks broadcaster) httpApplication

webSocketService :: MVar [Task] -> Broadcaster -> WebSocketServer Request Response
webSocketService tasks broadcaster request =
  case request of
    CreateTaskRequest name description -> do
      MVar.modifyMVar_ tasks (\allTasks -> return $ allTasks ++ [Task 1 name description Ready])
      newTasks <- MVar.readMVar tasks
      WS.broadcast broadcaster $ Response newTasks
      return Nothing
    LoadAllTasksRequest -> do
      newTasks <- MVar.readMVar tasks
      return $ Just $ Response newTasks

httpEndpoints :: ScottyM ()
httpEndpoints = do
  middleware $ staticPolicy (noDots >-> addBase "assets")
  matchAny "/" $ redirect "/index.html"
