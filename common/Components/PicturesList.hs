{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Components.PicturesList where

import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.Html (div_, h4_)
import Miso.Html.Property (class_)
import qualified Data.Vector as V
import qualified Data.Set as Set

import qualified Components.Picture as P
import qualified Components.CollectionControls as CC

type PicturesListComponent parent = Component parent Model Action

data Model = Model
    { picture_count :: Int
    , pictureInfo :: P.PicturesInfo
    , api_error :: Bool
    , pictureComponentIds :: Set.Set M.ComponentId
    }
    deriving Eq

data Action
    = ChangeCount Int
    | MountedPic
    | UnmountedPic
    | Initialize
    | OnControlsChange CC.OutMessage
    | OnMessageError MisoString


initialModel :: Model
initialModel = Model 6 V.empty False Set.empty


app :: Model -> PicturesListComponent parent
app initial_model =
    (M.component initial_model update view)
        { M.mount = Just MountedPic
        , M.unmount = Just UnmountedPic
        }


update :: Action -> Effect parent Model Action
update Initialize =
    M.subscribe CC.collectionControlsOutTopic OnControlsChange OnMessageError

update (OnControlsChange (CC.CountChanged newcount)) =
    issue $ ChangeCount newcount

update (OnMessageError err) =
    io_ $ consoleError ("Couldn't decode CollectionControls message: " <> toMisoString err)

update (ChangeCount new_count) =
    modify (\m -> m { picture_count = new_count })

update MountedPic = do
    name <- _componentInfoId <$> ask
    modify (f name)

    where
        f :: M.ComponentId -> Model -> Model
        f name model@(Model{ pictureComponentIds }) = 
            model { pictureComponentIds = Set.insert name pictureComponentIds }

update UnmountedPic = do
    name <- _componentInfoId <$> ask
    modify (f name)

    where
        f :: M.ComponentId -> Model -> Model
        f name model@(Model{ pictureComponentIds }) = 
            model { pictureComponentIds = Set.delete name pictureComponentIds }


view :: Model -> View Model Action
view (Model { api_error = True }) = h4_ [] [ text "API Error" ]
view (Model count pics_metadata False _) =
    div_
        [ class_ "picture-list" ]
        (map picture (take (min count (V.length pics_metadata)) [0..]))

    where
        picture :: Int -> View Model Action
        picture i = div_
            [ class_ "picture" ]
            [ mount_ $ P.app pics_metadata i ]
