{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Components.PicturesList where

import Control.Monad.State (modify)
import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.String (toMisoString)

import qualified Components.Picture as P

type PicturesListComponent = Component Model Action

data Model = Model
    { picture_count :: Int }
    deriving Eq
data Action = ChangeCount Int

initialModel :: Model
initialModel = Model 0

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

update :: Action -> Effect Model Action
update (ChangeCount new_count) =
    modify (\m -> m { picture_count = new_count })

view :: Model -> View Action
view (Model count) =
    div_
        []
        (map picture (take count [0..]))

    where
        picture :: Int -> View Action
        picture i =
            embed
                (component ("picture-" <> toMisoString i) $ P.app i)
                []
