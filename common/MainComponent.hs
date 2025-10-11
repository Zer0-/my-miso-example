{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module MainComponent where

import Miso
import Miso.Html
    ( div_
    , h1_
    , p_
    )
import qualified ChildComponent as C

type Action = ()

app :: App C.Model Action
app = Component
    { model = C.emptyModel
    , hydrateModel = Just $ return C.populatedModel
    , update = const $ pure ()
    , view = vview
    , subs = []
    , events = defaultEvents
    , styles = []
    , initialAction = Nothing
    , mountPoint = Nothing
    , logLevel = DebugAll
    --, logLevel = Off
    , scripts = []
    , mailbox = const Nothing
    , bindings = []
    }


vview :: C.Model -> View C.Model Action
vview  _ =
    div_ []
        [ h1_ [] [ "Example App" ]
        , p_ [] [ "Should only render each child once!" ]
        , mount (div_ []) C.app
        ]
