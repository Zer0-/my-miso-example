{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HttpClientTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)

import qualified HttpTypes as Http
import Miso.String (MisoString)

data Action a = Connect (Http.HttpActionResult a)

data Model = Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving Eq

data Interface a b = Interface
    { passAction :: Action b -> a
    , returnResult :: Http.HttpResult b -> a
    }
