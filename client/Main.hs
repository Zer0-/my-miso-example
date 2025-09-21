{-# LANGUAGE CPP #-}

module Main where

import Miso
    ( run
    , startComponent
    )
import Language.Javascript.JSaddle.Monad (JSM)
import Control.Monad.IO.Class (liftIO)
import MainApp (app)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

mainMain :: JSM ()
mainMain = do
    liftIO $ putStrLn "Hello World"

    (startComponent $ app)

main :: IO ()
main = run mainMain
