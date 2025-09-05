{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HttpClientTypes where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Miso.String (MisoString)

data PixabayResponse = PixabayResponse
    { total     :: Int
    , totalHits :: Int
    , hits      :: [PixabayImage]
    } deriving (Show, Generic, FromJSON, ToJSON)

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
    } deriving (Show, Generic, FromJSON, ToJSON, Eq)
