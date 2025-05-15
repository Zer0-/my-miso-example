{-# LANGUAGE OverloadedStrings #-}

module Components.MainComponent where

import Miso hiding (update, view)
import qualified Miso as M

import qualified Components.CollectionControls as CC
import qualified Components.PicturesList as PL

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
    , M.logLevel = M.DebugAll
    }

update :: Action -> Effect Model Action
update _ = noEff ()

view :: Model -> View Action
view _ =
    div_
        []
        [ div_
            [ class_ "topmatter" ]
            [ h1_ [ class_ "title" ] [ "Gfycat Demo" ]
            , p_ [ class_ "subtitle" ] [ "(Gfycat doesn't exist anymore so it's actually a Pixabay demo)" ]
            , embed controls [ class_ "collection-controls" ]
            ]
        , embed PL.pictureListComponent [ class_ "pictures-list" ]
        ]

    where
        controls = component "controls" (CC.app PL.pictureListComponent)
