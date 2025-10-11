{-# LANGUAGE OverloadedStrings #-}

module ChildComponent where

import Miso
import Miso.Html (div_, h3_)
import Miso.Lens (Lens (..))
import Miso.Html.Event (onClick)

data Model = Model { state :: [ MisoString ] }
    deriving Eq

emptyModel :: Model
emptyModel = Model []

populatedModel :: Model
populatedModel = emptyModel { state = testList }

testList :: [ MisoString ]
testList = [ "Child 1", "Child 2" ]

lens :: Lens Model [ MisoString ]
lens = Lens
    state
    (\x m -> m { state = x })

app :: Component Model Model MisoString
app = Component
    { model = emptyModel
    , hydrateModel = Just $ return populatedModel
    , update = uupdate
    , view = vview
    , subs = []
    , events = defaultEvents
    , styles = []
    , initialAction = Nothing
    , mountPoint = Nothing
    --, logLevel = Off
    , logLevel = DebugAll
    , scripts = []
    , mailbox = const Nothing
    , bindings = [ lens --> lens ]
    }

uupdate :: MisoString -> Effect parent Model MisoString
uupdate msg = io_ $ consoleLog $ "Child Component Clicked! " <> msg

vview :: Model -> View model MisoString
vview (Model xs) =
    div_
        []
        (map child xs)

    where
        child :: MisoString -> View model MisoString
        child label = div_ [ onClick label ] [ h3_ [] [ text label ] ]
