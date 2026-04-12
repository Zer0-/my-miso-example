{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Components.PicturesList where

import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.Html (div_, h4_)
import Miso.Html.Property (class_)
import Miso.JSON (Value)
import qualified Data.Vector as V
import qualified Data.Set as Set

import qualified Components.Picture as P
import qualified Components.CollectionControls as CC

import Debug.Trace (trace)

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
    | MountedPic M.ComponentId
    | UnmountedPic M.ComponentId
    | Initialize
    | OnControlsChange CC.OutMessage
    | OnMessageError MisoString
    | OnMailError MisoString


initialModel :: Model
initialModel = Model 6 V.empty False Set.empty


app :: Model -> PicturesListComponent parent
app initial_model =
    (M.component initial_model update view)
        { M.logLevel = M.DebugAll
        , M.mailbox = handleMail
        , M.mount = Just Initialize
        }

    where
        handleMail :: Value -> Maybe Action
        handleMail = M.checkMail actionFromChildMessage OnMailError
            where
                actionFromChildMessage :: P.PicMountStatusMsg -> Action
                actionFromChildMessage (P.PicMountStatusMsg True name) = MountedPic name
                actionFromChildMessage (P.PicMountStatusMsg False name) = UnmountedPic name


update :: Action -> Effect parent Model Action
update Initialize =
    M.subscribe CC.collectionControlsOutTopic OnControlsChange OnMessageError

update (OnControlsChange (CC.CountChanged newcount)) = do
    io_ $ consoleLog "PicturesList OnControlsChange"
    issue $ ChangeCount newcount

update (OnMessageError err) =
    io_ $ consoleError ("PicturesList couldn't decode CollectionControls message: " <> toMisoString err)

update (OnMailError err) =
    io_ $ consoleError ("PicturesList couldn't decode Picture message: " <> toMisoString err)

update (ChangeCount new_count) = trace "PicturesList ChangeCount" $
    modify (\m -> m { picture_count = new_count })

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


view :: Model -> View Model Action
view (Model { api_error = True }) = h4_ [] [ text "API Error" ]
view (Model count pics_metadata False _) = trace "PicturesList view function" $
    div_
        [ class_ "picture-list" ]
        (map picture (take (min count (V.length pics_metadata)) [0..]))

    where
        picture :: Int -> View Model Action
        picture i = div_
            [ class_ "picture" ]
            [ mount_ $ P.app pics_metadata i ]
