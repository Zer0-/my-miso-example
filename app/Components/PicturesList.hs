{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Components.PicturesList where

import Control.Monad (when)
import Control.Monad.State (modify, get)
import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.String (MisoString, toMisoString)
import qualified Data.Vector as V

import qualified Components.Picture as P
import qualified HttpClientTypes as Http
import qualified HttpClient as Http
import HttpTypes (HttpResult (HttpResponse))

type PicturesListComponent = Component Model Action

data Model = Model
    { picture_count :: Int
    , pictureInfo :: P.PicturesInfo
    , topic :: MisoString
    , api_error :: Bool
    }
    deriving Eq

data Action
    = Initialize
    | ChangeCount Int
    | ChangeTopic MisoString
    | ApiResponse (HttpResult Http.PixabayResponse)

initialModel :: Model
initialModel = Model 0 V.empty "Kitty Cats" False

app :: App Model Action
app = M.App
    { M.model = initialModel
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    }

pictureListComponent :: Component Model Action
pictureListComponent = component "pictures" app

update :: Action -> Effect Model Action
update (ChangeCount new_count) =
    modify (\m -> m { picture_count = new_count })
update (ChangeTopic t) = do
    model <- get
    let old_value = topic model

    when (old_value /= t) $ do
        modify (\m -> m { topic = t })
        issue Initialize

update Initialize = do
    model <- get

    io $ do
        consoleLog "Initialize PicturesList"
        notify http $
            Http.GetApiResults (topic model) $
            Http.Interface ApiResponse pictureListComponent

update (ApiResponse (HttpResponse _ _ (Just response))) = do
    io $ consoleLog ((toMisoString $ V.length vec) <> " pieces of image metadata obtained from API.")
    modify (\m -> m { pictureInfo = vec })

    where
        vec = V.fromList $ Http.hits response

update (ApiResponse _) = modify (\m -> m { api_error = True })


view :: Model -> View Action
view (Model { api_error = True }) = h4_ [] [ text "API Error" ]
view (Model count pics_metadata _ False) =
    div_
        []
        ( embed http [ onMounted Initialize ]
        : (map picture (take count [0..]))
        )

    where
        picture :: Int -> View Action
        picture i =
            embed
                (component ("picture-" <> toMisoString i) $ P.app pics_metadata i)
                []

http :: Component Http.Model (Http.Action Model Action Http.PixabayResponse)
http = component "http" Http.app
