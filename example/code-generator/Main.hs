{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Data.Proxy
import Elm.Export

spec :: Spec
spec =
  moduleSpec ["Api"] $ do
    renderType (Proxy :: Proxy Message)
    renderEncoder (Proxy :: Proxy Message)
    renderDecoder (Proxy :: Proxy Message)
    renderSubscriber

main :: IO ()
main = specsToDir [spec] "client/src"
