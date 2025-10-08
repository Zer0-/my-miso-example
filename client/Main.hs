{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Miso (run, miso)
import Language.Javascript.JSaddle.Monad (JSM)
import Control.Monad.IO.Class (liftIO)

import qualified MainComponent as MC

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

mainMain :: JSM ()
mainMain = do
    liftIO $ putStrLn "Hello World"

    miso $ const $ MC.app

main :: IO ()
main = run mainMain

-- TODO:
--      Render out new base64?
