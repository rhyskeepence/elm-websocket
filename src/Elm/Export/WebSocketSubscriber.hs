{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Export.WebSocketSubscriber
  ( renderSubscriber
  ) where

import           Elm.Export.Common
import           Elm.Export.Type
import           Elm.Export.Encoder (renderEncoderName)
import           Elm.Export.Decoder (renderDecoderName)
import           Text.PrettyPrint.Leijen.Text

{-|
  Render an Elm Subscriber which will listen and decode Responses of type a
-}
renderSubscriber :: ElmType a => ElmType b => a -> b -> RenderM ()
renderSubscriber requestType responseType = do
  require "WebSocket"
  require "Json.Decode exposing (Decoder, decodeString)"
  require "Result exposing (Result(..))"
  responseDecoderName <- renderDecoderName responseType
  responseDecoderType <- renderRef $ toElmType responseType
  requestEncoderName <- renderEncoderName requestType
  requestEncoderType <- renderRef $ toElmType requestType
  collectDeclaration . return $
    "listen : String -> (Result String" <+> responseDecoderType <+> "-> msg) -> Sub msg" <$$> "listen host tagger = " <$$>
    "    WebSocket.listen (\"ws://\" ++ host) (\\str -> decodeString" <+> responseDecoderName <+> "str |> tagger)" <$$>
    emptyline <$$>
    "send : String ->" <+> requestEncoderType <+> "-> Cmd msg" <$$>
    "send host value =" <$$>
    "    WebSocket.send (\"ws://\" ++ host) (Json.Encode.encode 0 (" <> requestEncoderName <+> "value))"



class HasTypeRef a where
  renderRef :: a -> RenderM Doc

instance HasTypeRef ElmDatatype where
  renderRef (ElmDatatype typeName _) = pure (stext typeName)
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasTypeRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = renderRef EString
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return $ "List" <+> parens dt
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $ dx <> comma <+> dy
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return $ "Maybe" <+> parens dt
  renderRef (EDict k v) = do
    require "Dict"
    dk <- renderRef k
    dv <- renderRef v
    return $ "Dict" <+> parens dk <+> parens dv
  renderRef EInt = pure "Int"
  renderRef EDate = do
    require "Date"
    pure "Date"
  renderRef EBool = pure "Bool"
  renderRef EChar = pure "Char"
  renderRef EString = pure "String"
  renderRef EUnit = pure "()"
  renderRef EFloat = pure "Float"