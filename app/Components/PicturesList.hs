{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Components.PicturesList where

import Control.Monad (when)
import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.String (MisoString, toMisoString)
import qualified Data.Vector as V
import qualified Data.Set as Set

import qualified Components.Picture as P
import qualified HttpClientTypes as Http
import qualified HttpClient as Http
import HttpTypes (HttpResult (HttpResponse))

type PicturesListComponent = Component "pictures-list" Model Action

data Model = Model
    { picture_count :: Int
    , pictureInfo :: P.PicturesInfo
    , topic :: MisoString
    , api_error :: Bool
    , pictureComponentIds :: Set.Set MisoString
    }
    deriving Eq

data Action
    = Initialize
    | ChangeCount Int
    | ChangeTopic MisoString
    | ApiResponse (HttpResult Http.PixabayResponse)
    | MountedPic MisoString
    | UnmountedPic MisoString


initialModel :: Model
initialModel = Model 6 V.empty "Kitty Cats" False Set.empty


app :: Component "pictures-list" Model Action
app = M.Component
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

    io_ $ do
        consoleLog "Initialize PicturesList"
        notify http $
            Http.GetApiResults (topic model) $
            Http.Interface ApiResponse app

update (ApiResponse (HttpResponse _ _ (Just response))) = do
    io_ $ consoleLog $
        (toMisoString $ V.length vec) <>
            " pieces of image metadata obtained from API."

    model <- get
    put model{ pictureInfo = vec }

    io_ $
        mapM_
            (\picId -> notify' picId (P.ChangeInfo vec))
            (pictureComponentIds model)

    where
        vec = V.fromList $ Http.hits response

update (ApiResponse _) = modify (\m -> m { api_error = True })

update (MountedPic name) =
    modify f

    where
        f :: Model -> Model
        f model@(Model{ pictureComponentIds }) = 
            model { pictureComponentIds = Set.insert name pictureComponentIds }

update (UnmountedPic name) =
    modify f

    where
        f :: Model -> Model
        f model@(Model{ pictureComponentIds }) = 
            model { pictureComponentIds = Set.delete name pictureComponentIds }


view :: Model -> View Action
view (Model { api_error = True }) = h4_ [] [ text "API Error" ]
view (Model count pics_metadata _ False _) =
    div_
        [ class_ "picture-list" ]
        ( componentWith http Nothing [ class_ "hidden", onMounted Initialize ]
        : (map picture (take (min count (V.length pics_metadata)) [0..]))
        )

    where
        picture :: Int -> View Action
        picture i = componentWith_
            (P.app pics_metadata i)
            (Just $ Key $ "picture-" <> toMisoString i)
            [ class_ "picture"

            , onMountedWith MountedPic
            , onUnmountedWith UnmountedPic
            ]


http :: Component "http-client" Http.Model (Http.Action "pictures-list" Model Action Http.PixabayResponse)
http = Http.app
