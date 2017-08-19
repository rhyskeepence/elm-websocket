{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module RequestResponseSpec where

import           Elm.WebSocket
import           Test.Hspec

import           Control.Concurrent           (forkIO)
import qualified Control.Concurrent.Broadcast as B
import qualified Control.Concurrent.Event     as E
import           Control.Monad                (forever)
import           Data.Aeson                   (FromJSON, ToJSON, decode, encode)
import           Data.Text.Lazy               (Text)
import           Data.Text.Lazy.Encoding      (decodeUtf8, encodeUtf8)
import           GHC.Generics                 (Generic)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp     (testWithApplication)
import qualified Network.WebSockets           as WS

data Message
  = Request { question :: String}
  | Response { answer :: String}
  deriving (Eq, Show, Generic)

instance ToJSON Message

instance FromJSON Message

type Requests = B.Broadcast Text

type Responses = B.Broadcast Text

type TestState = (Requests, Responses)


spec :: Spec
spec = requestResponseSpec


requestResponseSpec :: Spec
requestResponseSpec =
  around runWithClientServer $
    describe "WebSocket Server" $
      it "should respond to requests" $ \(requests, responses) -> do
        sendRequest requests (Request "hi friend")
        message <- waitForResponse responses
        message `shouldBe` Just (Response "hi friend")


sendRequest :: ToJSON a => Requests -> a -> IO ()
sendRequest requests message = B.broadcast requests $ decodeUtf8 $ encode message


waitForResponse :: Responses -> IO (Maybe Message)
waitForResponse responses = do
  json <- B.listenTimeout responses 100000
  return $ json >>= (decode . encodeUtf8)


runWithClientServer :: (TestState -> IO a) -> IO a
runWithClientServer action = do
  broadcaster <- newBroadcaster
  requests <- B.new
  responses <- B.new
  isConnected <- E.new
  testWithApplication (return $ withWebSocketBroadcaster broadcaster webSocketService httpApplication) $ \port -> do
    _ <- forkIO $ WS.runClient "localhost" port "" $ client requests responses isConnected
    E.wait isConnected
    action (requests, responses)
  where

    client :: Requests -> Responses -> E.Event -> WS.Connection -> IO ()
    client requests responses isConnected connection = do
      E.set isConnected
      forever $
        do request <- B.listen requests
           WS.sendTextData connection request
           WS.receiveData connection >>= B.broadcast responses

    webSocketService :: WebSocketServer Message Message
    webSocketService (Request message) = return $ Just (Response message)
    webSocketService _ = return Nothing

    httpApplication :: Application
    httpApplication _ respond = respond $ responseLBS Network.HTTP.Types.status400 [] "Not a WebSocket request"
