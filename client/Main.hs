{-# LANGUAGE CPP #-}

module Main where

import Miso (run, startComponent)
import qualified Components.MainComponent as MC

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run (startComponent MC.app)
