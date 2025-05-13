{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HttpClientTypes where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

import qualified HttpTypes as Http
import Miso.String (MisoString)
import Miso (Component)

data Action m a b
    = Connect (Interface m a b) (Http.HttpActionResult b)
    | GetApiResults MisoString (Interface m a b)

data Model = Model
  { apiRoot :: MisoString
  , apiKey :: MisoString
  } deriving Eq

data Interface m a b = Interface
    { returnResult :: Http.HttpResult b -> a
    , notifyComponent :: Component m a
    }

data PixabayResponse = PixabayResponse
  { total     :: Int
  , totalHits :: Int
  , hits      :: [PixabayImage]
  } deriving (Show, Generic, FromJSON)

data PixabayImage = PixabayImage
    { id              :: Int
    , previewWidth    :: Int
    , previewHeight   :: Int
    , previewURL      :: MisoString
    , webformatWidth  :: Int
    , webformatHeight :: Int
    , webformatURL    :: MisoString
    , largeImageURL   :: MisoString
    , imageWidth      :: Int
    , imageHeight     :: Int
    } deriving (Show, Generic, FromJSON, Eq)
