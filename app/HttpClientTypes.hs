{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HttpClientTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)

import qualified HttpTypes as Http
import Miso.String (MisoString)

type ReturnResult = Http.HttpResult b -> a

data Action a = Connect ReturnResult (Http.HttpActionResult a)

data Model = Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving Eq
