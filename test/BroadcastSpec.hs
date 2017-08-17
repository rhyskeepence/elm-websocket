{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BroadcastSpec where

import           Elm.WebSocket
import           Test.Hspec

import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.Broadcast as B
import qualified Control.Concurrent.Event     as E
import           Control.Monad                (forever)
import           Data.Aeson                   (ToJSON)
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

type ReceivedMessage = B.Broadcast Text

type TestState = (ConnectedClientsState, ReceivedMessage)

spec :: Spec
spec = broadcastSpec

broadcastSpec :: Spec
broadcastSpec = around runWithClientServer $
  describe "Broadcaster" $
    it "should broadcast a message to connected clients" $ \(clients, messages) -> do
      _ <- broadcast clients $ Message "broadcast"
      message <- B.listenTimeout messages 100000
      message `shouldBe` Just "{\"hello\":\"broadcast\"}"


runWithClientServer :: (TestState -> IO a) -> IO a
runWithClientServer action = do
  clients <- newConnectedClientsState
  messages <- B.new
  isConnected <- E.new

  testWithApplication (return $ withWebSocketBroadcaster clients httpApplication) $ \port -> do
      _ <- forkIO $ WS.runClient "localhost" port "" $ clientReceiver messages isConnected
      E.wait isConnected
      action (clients, messages)

  where

    clientReceiver :: ReceivedMessage -> E.Event -> WS.Connection -> IO ()
    clientReceiver receivedMessageBroadcast isConnected connection = do
      E.set isConnected
      _ <- forever $ WS.receiveData connection >>= B.broadcast receivedMessageBroadcast
      return ()

    httpApplication :: Application
    httpApplication _ respond = respond $ responseLBS Network.HTTP.Types.status400 [] "Not a WebSocket request"
