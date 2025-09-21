{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Miso.String (MisoString)
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

type UrlParseError = String


data ParsedURL = ParsedURL
  { boardName   :: String
  , threadId    :: Maybe Integer
  , postId      :: Maybe Integer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


data PostPart
    = SimpleText MisoString
    | PostedUrl MisoString
    | Skip
    | Quote (Either UrlParseError ParsedURL)
        -- Quotes don't seem to be able to be spoilered
        -- board links (which appear as quotes but start with >>>) break the tag
    | GreenText     [ PostPart ]
    | OrangeText    [ PostPart ]
    | RedText       [ PostPart ]
    | Spoiler       [ PostPart ]
    -- you can't seem to spoiler greentext
    | Bold          [ PostPart ]
    | Underlined    [ PostPart ]
    | Italics       [ PostPart ]
    | Strikethrough [ PostPart ]
    | Code          [ PostPart ]
    deriving (Show, Eq, Generic, FromJSON, ToJSON)


data Site = Site
  { opening_post_board_id :: Integer
  , name :: MisoString
  , board :: MisoString
  }
