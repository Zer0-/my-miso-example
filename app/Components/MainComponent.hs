{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Components.MainComponent where

import Miso hiding (update, view)
import qualified Miso as M

type Model = ()
type Action = ()

initialModel :: Model
initialModel = ()

app :: Component "main-app" Model Action
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
update _ = return ()

view :: Model -> View Action
view _ =
    div_
        [ class_ "topmatter" ]
        [ h1_ [ class_ "title" ] [ "Bug Demo" ]
        , p_ [ class_ "subtitle" ] [ "Bugs are bad." ]
        ]
