{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Api where

import Elm.Export
import           GHC.Generics                  (Generic)
import           Data.Aeson                    (FromJSON, ToJSON)

data Message = SparklineUpdate
  { newValue :: Int
  } deriving (Eq, Show, Generic, ElmType)

instance ToJSON Message

instance FromJSON Message
