{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module JSFFI.MisoFFI
    ( Document (..)
    , Element (..)
    , ParentNode (..)
    , getDocument
    , querySelector
    , getAttribute
    , textContent
    , alert
    ) where

import Miso.String (MisoString, fromMisoString)
import Miso.DSL
    ( JSVal
    , jsg
    , (#)
    , (!)
    , isNull
    , isUndefined
    , fromJSVal
    )
import Control.Lens.Operators ((^.))

newtype Document = Document JSVal
newtype Element = Element JSVal
newtype ParentNode = ParentNode JSVal

-- | Safely checks if a JSVal is null or undefined
maybeNullOrUndefined :: JSVal -> IO (Maybe JSVal)
maybeNullOrUndefined x = do
    nullYes <- isNull x
    if nullYes
        then return Nothing
        else do
            undefYes <- isUndefined x
            if undefYes then return Nothing else return (Just x)

getDocument :: IO Document
getDocument = Document <$> jsg ("document" :: MisoString)

querySelector :: ParentNode -> MisoString -> IO (Maybe Element)
querySelector (ParentNode n) s =
    (Element <$>) <$> ((n # ("querySelector" :: MisoString) $ [s]) >>= maybeNullOrUndefined)

getAttribute1 :: JSVal -> MisoString -> IO (Maybe JSVal)
getAttribute1 x attr =
    (x # ("getAttribute" :: MisoString) $ [attr]) >>= maybeNullOrUndefined


textContent :: Element -> IO (Maybe MisoString)
textContent (Element e) = e ! "textContent" >>= fromJSVal


getAttribute :: JSVal -> MisoString -> IO (Maybe MisoString)
getAttribute x attr = do
    mVal <- (x # "getAttribute" $ [attr]) >>= maybeNullOrUndefined
    case mVal of
        Nothing -> return Nothing
        Just v  -> fromJSVal v >>= \case
            Just s  -> return (Just s)
            Nothing -> fail $ "Attribute '" ++ fromMisoString attr ++ "' returned non-string value"


alert :: MisoString -> IO ()
alert msg = do
    alertFunc <- jsg ("alert" :: MisoString)  -- Get the global alert function
    _ <- alertFunc # ("" :: MisoString) $ [msg]                -- Invoke with the message
    return ()
