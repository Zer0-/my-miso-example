{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Miso (View)
import Servant.API

import Components.Types (Action)

type Home = View Action

type Flip = "clicked" :> View Action

type Route = Home :<|> Flip
