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
webSocketService LoadAllTasksRequest = return $ Just (LoadAllTasksResponse
  [ Task 1 "example 1" "This had me run into my first big problem. How do I deal with the enforced purity of Haskell and IO. If I have understood the desired design correctly I ideally have an IO main function which then progressively has less or ideally none IO as I 'go down' the hierarchy of other functions. This seems problematic because of the event based terminal library that in this case is supposed to constantly read the directory(IO Filepath). I would love to have a separate function that just gets the IO Filepath and returns me a non-IO list, but that clearly defeats the haskell design as the IO taints everything." Ready
  , Task 2 "Another Example" "Don't feel like your code needs to be pure. It's totally fine to write code that has a ton of IO, especially at first. We require that impure code use the IO type, but we don't require that you only use pure code. For \"batch\" programs, it's really easy to avoid IO for the inner logic. You have some function:" InPlay ])
webSocketService _ = return Nothing


httpEndpoints :: ScottyM ()
httpEndpoints = do
  middleware $ staticPolicy (noDots >-> addBase "assets")
  matchAny "/" $ redirect "/index.html"
