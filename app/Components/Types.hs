{-# LANGUAGE OverloadedStrings #-}

module Components.Types where

type Model = ()
data Action = Initialize | Click | Mounted | Unmounted
