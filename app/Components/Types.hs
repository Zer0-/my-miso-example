{-# LANGUAGE OverloadedStrings #-}

module Components.Types where

import Miso (URI, Topic, topic)
import Data.Aeson (Result (..))

type Model = URI
data Action
    = Initialize
    | Clicked (Result Message)
    | HeaderMounted
    | HeaderUnmounted

type Message = ()

clickTopic :: Topic Message
clickTopic = topic "click"
