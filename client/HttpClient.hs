{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpClient
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action (..)
    , Interface (..)
    , Model (..)
    , update
    , app
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Data.Aeson (ToJSON, FromJSON)
import Data.JSString (replace)
import Control.Monad.State (get)
import GHC.TypeLits (KnownSymbol)

import Miso (withSink, Effect, JSM, io_, notify, Component, text)
import qualified Miso as M
import Miso.String (MisoString)
import Language.Javascript.JSaddle.Monad (askJSM, runJSaddle)

import qualified Http
import HttpClientTypes

awaitResult
    :: KnownSymbol n
    => Interface n m a b
    -> Http.HttpActionResult b
    -> Effect Model (Action n m a b)
awaitResult iface (_, resultVar) = do
    io_ $ do
        ctx <- askJSM

        void $ liftIO $ forkIO $ do
            result :: Http.HttpResult b <- takeMVar resultVar
            --runJSaddle ctx $ sink $ (returnResult iface) result
            runJSaddle ctx $
                notify (notifyComponent iface) $ (returnResult iface) result

update :: (FromJSON b, KnownSymbol n) => Action n m a b -> Effect Model (Action n m a b)
update (GetApiResults topic iface) = do
    io_ $ M.consoleLog "HttpClient - GetApiResults"

    model <- get

    withSink $ \sink ->
        http_ model (path model) Http.GET (Nothing :: Maybe Int)
        >>= sink . Connect iface

    where
        path :: Model -> MisoString
        path model = "/api/?key=" <> apiKey model <> "&q=" <> q <> "&image_type=photo"

        q :: MisoString
        q = replace " " "+" topic

update (Connect iface actionResult) = awaitResult iface actionResult

http_
    :: (ToJSON a, FromJSON b)
    => Model
    -> MisoString
    -> Http.HttpMethod
    -> Maybe a
    -> JSM (Http.HttpActionResult b)
http_ m apiPath method payload =
    Http.http
        (apiRoot m <> apiPath)
        method
        [("Content-Type", "application/json")]
        payload


app :: (FromJSON b, KnownSymbol n) => Component name Model (Action n m a b)
app = M.Component
                    -- localhost:8881 is the proxied pixabay URL served by static/serve.py. It's proxied because CORS policy won't let the browser hit it.
    { M.model = Model "" "50180908-cd7347b4d526def52ed2faf77"
    , M.update = update
    , M.view = const $ text ""
    , M.subs = []
    , M.events = M.defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    }
