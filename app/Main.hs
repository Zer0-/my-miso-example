{-# LANGUAGE CPP #-}

module Main where

import Miso (run, startApp)
import qualified Components.MainComponent as MC

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run (startApp MC.app)
