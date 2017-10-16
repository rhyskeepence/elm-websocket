{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Data.Proxy
import Elm.Export

spec :: Spec
spec =
  Spec ["Api"]
  [ toElmTypeSource    (Proxy :: Proxy Message)
  , toElmDecoderSource (Proxy :: Proxy Message)
  , toElmEncoderSource (Proxy :: Proxy Message)
  ]

main :: IO ()
main = specsToDir [spec] "client/src"
