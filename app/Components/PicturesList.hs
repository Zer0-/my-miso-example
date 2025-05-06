{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Components.PicturesList where

import Miso hiding (update, view, model)
import qualified Miso as M

import qualified Components.Picture as P

data Model = Model [ App P.Model P.Action ]
    deriving Eq
data Action = Add Int | Remove Int

initialModel :: Model
initialModel = Model []

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
update _ = noEff initialModel

view :: Model -> View Action
view _ =
    div_
        []
        [ text "Images" ]
