{-# LANGUAGE OverloadedStrings #-}

module Components.MainComponent where

import Miso hiding (update, view)
import qualified Miso as M

type Model = ()
type Action = ()

initialModel :: Model
initialModel = ()

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
    , M.logLevel = M.Off
    }


update :: Action -> Effect Model Action
update _ = noEff ()

view :: Model -> View Action
view _ =
    h1_ [] [ "Hello World" ]
