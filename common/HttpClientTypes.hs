{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HttpClientTypes where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

import qualified HttpTypes as Http
import Miso.String (MisoString)
import Miso (Component)

data Action n m a b
    = Connect (Interface n m a b) (Http.HttpActionResult b)
    | GetApiResults MisoString (Interface n m a b)

data Model = Model
    { apiRoot :: MisoString
    , apiKey :: MisoString
    } deriving Eq

data Interface n m a b = Interface
    { returnResult :: Http.HttpResult b -> a
    , notifyComponent :: Component n m a
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
