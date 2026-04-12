{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Miso (miso, defaultEvents, withJS)
import Miso.String (MisoString, fromMisoString, toMisoString)
import Control.Monad.IO.Class (liftIO)
import Miso.JSON (decode)
import qualified Data.Vector as V

import qualified Components.MainComponent as MC
import JSFFI.MisoFFI
    ( getDocument
    , Element (..)
    , Document (..)
    , ParentNode (..)
    , querySelector
    , textContent
    , alert
    )
import HttpClientTypes (PixabayResponse, hits)
import qualified Components.PicturesList as PL

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

getScriptContents :: MisoString -> IO (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e

main :: IO ()
main = withJS $ do
    liftIO $ putStrLn "Hello World"

    raw_initial_data <- getScriptContents "initial-data"

    let decoded_response :: Maybe PixabayResponse = decode =<< raw_initial_data

    case decoded_response of
            Nothing -> alert("Unable to find or decode sample data needed to draw the page. Ensure that an element with class .initial-data exists and contains well-formatted JSON")
            Just sample_response -> do
                let pl_model = PL.initialModel
                        { PL.pictureInfo = V.fromList (hits sample_response) }

                let pl = PL.app pl_model

                (miso defaultEvents $ const $ MC.app pl)
