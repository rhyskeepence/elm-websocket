{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Api where

import Elm.Export
import           GHC.Generics                  (Generic)
import           Data.Aeson                    (FromJSON, ToJSON)
import Data.Text


data TaskStatus
  = Ready
  | InPlay
  | Done
  deriving (Eq, Show, Generic, ElmType)

instance ToJSON TaskStatus

instance FromJSON TaskStatus

data Task
  = Task
  { id :: Int
  , name :: Text
  , description :: Text
  , status :: TaskStatus
  } deriving (Eq, Show, Generic, ElmType)

instance ToJSON Task

instance FromJSON Task


data Message
  = CreateTaskRequest Text Text
  | LoadAllTasksRequest
  | LoadAllTasksResponse [Task]
  deriving (Eq, Show, Generic, ElmType)


instance ToJSON Message

instance FromJSON Message
