{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BroadcastSpec where

import           Elm.WebSocket
import           Test.Hspec

import           Control.Concurrent           (forkIO)
import qualified Control.Concurrent.Broadcast as B
import           Control.Exception            (bracket)
import           Control.Monad                (forever)
import           Control.Monad.Trans          (liftIO)
import           Data.Aeson                   (ToJSON)
import           Data.Text                    (Text)
import qualified Data.Text.IO                 as T
import           GHC.Generics                 (Generic)
import           Network.HTTP.Types
import           Network.Socket               (withSocketsDo)
import           Network.Wai
import           Network.Wai.Handler.Warp     (run)
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
broadcastSpec =
  around runWithClientServer $
  do describe "Broadcaster" $
       do it "should broadcast a message to connected clients" $
            \(clients, messages) -> do broadcast clients $ Message "broadcast"

runWithClientServer :: (TestState -> IO ()) -> IO ()
runWithClientServer = bracket setupClientServer tearDownClientServer

setupClientServer :: IO TestState
setupClientServer = do
  clients <- newConnectedClientsState
  messagesReceived <- B.new
  _ <- forkIO $ run 10000 $ withWebSocketBroadcaster clients app
  _ <- withSocketsDo $ WS.runClient "localhost" 10000 "/" clientReceiver
  return (clients, messagesReceived)
  where
    clientReceiver connection =
      forkIO $
      forever $
      do msg <- WS.receiveData connection
         liftIO $ T.putStrLn msg

app :: Application
app req f = f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

tearDownClientServer :: TestState -> IO ()
tearDownClientServer _ = return ()
