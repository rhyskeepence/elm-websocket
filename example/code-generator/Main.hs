{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Data.Proxy
import Elm.Export

spec :: Spec
spec =
  moduleSpec ["Api"] $ do
    renderType (Proxy :: Proxy TaskStatus)
    renderType (Proxy :: Proxy Task)
    renderType (Proxy :: Proxy Message)
    renderEncoder (Proxy :: Proxy TaskStatus)
    renderEncoder (Proxy :: Proxy Task)
    renderEncoder (Proxy :: Proxy Message)
    renderDecoder (Proxy :: Proxy TaskStatus)
    renderDecoder (Proxy :: Proxy Task)
    renderDecoder (Proxy :: Proxy Message)
    renderSubscriber

main :: IO ()
main = specsToDir [spec] "client/src"
