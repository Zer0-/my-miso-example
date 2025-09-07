{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Miso (run, miso)
import Miso.String (MisoString, fromMisoString, toMisoString)
import Language.Javascript.JSaddle.Monad (JSM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decodeStrict)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V

import qualified Components.MainComponent as MC
import JSFFI.Saddle
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

getScriptContents :: MisoString -> JSM (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e

mainMain :: JSM ()
mainMain = do
    liftIO $ putStrLn "Hello World"

    raw_initial_data <- getScriptContents "initial-data"

    let decoded_response :: Maybe PixabayResponse =
            (decodeStrict . encodeUtf8 . fromMisoString) =<< raw_initial_data

    case decoded_response of
            Nothing -> alert("Unable to find or decode sample data needed to draw the page. Ensure that an element with class .initial-data exists and contains well-formatted JSON")
            Just sample_response -> do
                let pl_model = PL.initialModel
                        { PL.pictureInfo = V.fromList (hits sample_response) }

                let pl = PL.app pl_model

                (miso $ const $ MC.app pl)

main :: IO ()
main = run mainMain
