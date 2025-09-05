{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Components.MainComponent where

import Miso hiding (update, view)
import qualified Miso as M
import Miso.Html
    ( div_
    , h1_
    , p_
    )
import Miso.Html.Property
    ( class_
    )

import qualified Components.CollectionControls as CC
import qualified Components.PicturesList as PL
import ApplicationTypes (Model (..), Action)

initialModel :: Model
initialModel = Model ()

app :: PL.PicturesListComponent Model -> App Model Action
app pl = M.Component
    { M.model = initialModel
    , M.update = update
    , M.view = view pl
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    , M.bindings = []
    }

update :: Action -> Effect parent Model Action
update _ = return ()

view :: PL.PicturesListComponent Model -> Model -> View Model Action
view pl _ =
    div_
        []
        [ div_
            [ class_ "topmatter" ]
            [ h1_ [ class_ "title" ] [ "Gfycat Demo" ]
            , p_ [ class_ "subtitle" ] [ "(Gfycat doesn't exist anymore so it's actually a Pixabay demo)" ]
            , mount (div_ [ class_ "collection-controls" ] ) CC.app
            ]
        , mount (div_ [ class_ "pictures-list" ]) pl
        ]
