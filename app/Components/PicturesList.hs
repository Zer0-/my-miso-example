{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Components.PicturesList where

import Control.Monad.State (modify)
import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.String (toMisoString)
import Data.Vector (Vector, empty)

import qualified Components.Picture as P
import qualified HttpClientTypes as Http
import qualified HttpClient as Http

type PicturesListComponent = Component Model Action

data Model = Model
    { picture_count :: Int
    , pictureInfo :: Vector Http.PixabayImage
    }
    deriving Eq

data Action = Initialize | ChangeCount Int

initialModel :: Model
initialModel = Model 0 empty

app :: App Model Action
app = M.App
    { M.model = initialModel
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Just Initialize
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    }

update :: Action -> Effect Model Action
update (ChangeCount new_count) =
    modify (\m -> m { picture_count = new_count })

-- update Initialize = io getApiResults

view :: Model -> View Action
view (Model count pics_metadata) =
    div_
        []
        ( embed http []
        : (map picture (take count [0..]))
        )

    where
        picture :: Int -> View Action
        picture i =
            embed
                (component ("picture-" <> toMisoString i) $ P.app i)
                []

http :: Component Http.Model (Http.Action Http.Model Action Http.PixabayResponse)
http = component "http" Http.app
