{-# LANGUAGE OverloadedStrings #-}

module JSFFI.XHR
    ( XMLHttpRequest (..)
    , abort
    , send
    , setRequestHeader
    , open
    , newXMLHttpRequest
    , addEventListener
    , getStatusText
    , getResponseText
    , getStatus
    ) where

import Data.Maybe (fromJust)
import Language.Javascript.JSaddle
    ( JSVal
    , JSM
    , JSString
    , (#)
    , ToJSVal (..)
    , jsg
    , (!)
    , new
    , asyncFunction
    , FromJSVal (..)
    )


newtype XMLHttpRequest = XMLHttpRequest JSVal


newXMLHttpRequest :: JSM XMLHttpRequest
newXMLHttpRequest = XMLHttpRequest <$> new (jsg ("XMLHttpRequest" :: JSString)) ()


abort :: XMLHttpRequest -> JSM ()
abort (XMLHttpRequest xhr) = do
    _ <- xhr # ("abort" :: JSString) $ ([] :: [ JSString ])
    return ()


send :: (ToJSVal a) => XMLHttpRequest -> a -> JSM ()
send (XMLHttpRequest xhr) payload = do
    _ <- xhr # ("send" :: JSString) $ ([ toJSVal payload ])
    return ()


setRequestHeader :: XMLHttpRequest -> JSString -> JSString -> JSM ()
setRequestHeader (XMLHttpRequest xhr) k v = do
    _ <- xhr # ("setRequestHeader" :: JSString) $ ([ k, v ])
    return ()


open :: XMLHttpRequest -> JSString -> JSString -> JSM ()
open (XMLHttpRequest xhr) method url = do
    _ <- xhr # ("open" :: JSString) $ ([ method, url ])
    return ()


getStatusText :: XMLHttpRequest -> JSM JSString
getStatusText (XMLHttpRequest xhr) =
    getProp_ xhr "statusText" >>= return . fromJust


getResponseText :: XMLHttpRequest -> JSM (Maybe JSString)
getResponseText (XMLHttpRequest xhr) = getProp_ xhr "responseText"


getStatus :: XMLHttpRequest -> JSM Int
getStatus (XMLHttpRequest xhr) = getProp_ xhr "status" >>= return . fromJust


addEventListener
  :: JSVal
  -> JSString
  -> JSM ()
  -> JSM ()
addEventListener self name cb = do
    _ <- self # ("addEventListener" :: JSString) $ (name, asyncFunction handle)
    return ()

    where
      handle _ _ _ = cb


getProp_ :: (FromJSVal a) => JSVal -> JSString -> JSM (Maybe a)
getProp_ self name = self ! name >>= fromJSVal
