{-# LANGUAGE OverloadedStrings #-}

module Components.Picture where

import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.String (toMisoString)

type Model = Int
type Action = ()

app :: Int -> App Model Action
app i = M.App
    { M.model = i
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
update _ = io $ return ()

view :: Model -> View Action
view i =
    div_
        []
        [ h1_ [] [ text $ "Image" <> toMisoString i ]
        ]
