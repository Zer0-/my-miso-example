{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Miso
    ( miso
    , defaultEvents
    , JSVal
    , alert
    , fromJSVal
    , (!)
    , isUndefined
    , isNull
    , (#)
    , jsg
    )
import Miso.JSON (decode)
import Miso.String (MisoString, fromMisoString, toMisoString)
import qualified Data.Vector as V

import qualified Components.MainComponent as MC
import HttpClientTypes (PixabayResponse, hits)
import qualified Components.PicturesList as PL

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

newtype Document = Document JSVal
newtype Element = Element JSVal
newtype ParentNode = ParentNode JSVal

getDocument :: IO Document
getDocument = Document <$> jsg ("document" :: MisoString)

querySelector :: ParentNode -> MisoString -> IO (Maybe Element)
querySelector (ParentNode n) s =
    (Element <$>) <$> ((n # "querySelector" $ [s]) >>= maybeNullOrUndefined)

    where
        maybeNullOrUndefined x = do
            nullYes <- isNull x

            if nullYes then
                return Nothing
            else do
                undefYes <- isUndefined x

                if undefYes then
                    return Nothing
                else
                    return $ Just x


textContent :: Element -> IO (Maybe MisoString)
textContent (Element e) = e ! "textContent" >>= fromJSVal

getScriptContents :: MisoString -> IO (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e

main :: IO ()
main = do
    putStrLn "Hello World"

    raw_initial_data <- getScriptContents "initial-data"

    let decoded_response :: Maybe PixabayResponse =
            decode =<< raw_initial_data

    case decoded_response of
        Nothing -> alert("Unable to find or decode sample data needed to draw the page. Ensure that an element with class .initial-data exists and contains well-formatted JSON")
        Just sample_response -> do
            let pl_model = PL.initialModel
                    { PL.pictureInfo = V.fromList (hits sample_response) }

            let pl = PL.app pl_model

            (miso defaultEvents $ const $ MC.app pl)
