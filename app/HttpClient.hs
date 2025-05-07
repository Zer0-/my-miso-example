{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module HttpClient
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action (..)
    , Interface (..)
    , fetchLatest
    , getThread
    , Model (..)
    , update
    , search
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)

import Miso (effectSub, Effect, JSM)
import Miso.String (MisoString, toMisoString)
import Language.Javascript.JSaddle.Monad (askJSM, runJSaddle)

import qualified Http
import HttpClientTypes

update
    :: Action b
    -> Model
    -> Effect Model a
update (Connect (_, resultVar)) m =
    effectSub m $ \sink -> do
        ctx <- askJSM

        void $ liftIO $ forkIO $ do
            result :: Http.HttpResult b <- takeMVar resultVar
            runJSaddle ctx $ sink $ (returnResult iface) result

http_
    :: (ToJSON c, FromJSON b)
    => Model
    -> MisoString
    -> Http.HttpMethod
    -> Maybe c
    -> JSM a
http_ m api_path method payload =
    Http.http
        (pgApiRoot m <> api_path)
        method
        [("Content-Type", "application/json")]
        payload
    >>= return . (passAction iface) . Connect
