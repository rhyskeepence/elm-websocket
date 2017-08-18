{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BroadcastSpec where

import           Elm.WebSocket
import           Test.Hspec

import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.Broadcast as B
import qualified Control.Concurrent.Event     as E
import           Control.Monad                (forever)
import           Data.Aeson                   (ToJSON, FromJSON)
import           Data.Text (Text)
import           GHC.Generics                 (Generic)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp     (testWithApplication)
import qualified Network.WebSockets           as WS

newtype Message = Message
  { hello :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

type ReceivedMessage = B.Broadcast Text

type TestState = (Broadcaster, ReceivedMessage)

spec :: Spec
spec = broadcastSpec

broadcastSpec :: Spec
broadcastSpec = around runWithClientServer $
  describe "Broadcaster" $
    it "should broadcast a message to connected clients" $ \(broadcaster, messages) -> do
      _ <- broadcast broadcaster $ Message "broadcast"
      message <- B.listenTimeout messages 100000
      message `shouldBe` Just "{\"hello\":\"broadcast\"}"


runWithClientServer :: (TestState -> IO a) -> IO a
runWithClientServer action = do
  broadcaster <- newBroadcaster
  messages <- B.new
  isConnected <- E.new

  testWithApplication (return $ withWebSocketBroadcaster broadcaster webSocketService httpApplication) $ \port -> do
      _ <- forkIO $ WS.runClient "localhost" port "" $ clientReceiver messages isConnected
      E.wait isConnected
      action (broadcaster, messages)

  where

    clientReceiver :: ReceivedMessage -> E.Event -> WS.Connection -> IO ()
    clientReceiver receivedMessageBroadcast isConnected connection = do
      E.set isConnected
      _ <- forever $ WS.receiveData connection >>= B.broadcast receivedMessageBroadcast
      return ()

    webSocketService :: WebSocketServer Message Message
    webSocketService _ = Nothing

    httpApplication :: Application
    httpApplication _ respond = respond $ responseLBS Network.HTTP.Types.status400 [] "Not a WebSocket request"
