{-# LANGUAGE OverloadedStrings #-}

module Components.MainComponent where

import Miso hiding (update, view, model)
import qualified Miso as M

import Components.Types

initialModel :: Model
initialModel = ()


app :: Component Model Action
app = M.Component
    { M.model = initialModel
    , M.update = update
    , M.view = const view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Just Initialize
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    }


update :: Action -> Effect Model Action
update Initialize = do
    io_ $ consoleLog "1"
    io_ $ consoleLog "2"
update Click = io_ $ consoleLog "Clicked"
update Mounted = io_ $ consoleLog "home Mounted"
update Unmounted = io_ $ consoleLog "home Unmounted"


view :: View Action
view = div_
    [ onMounted Mounted
    , onUnmounted Unmounted
    , class_ "topmatter"
    ]
    [ h1_ [ class_ "title" ] [ "Bug Demo" ]
    , p_ [ class_ "subtitle" ] [ "Bugs are bad." ]
    , button_
        [ onClick Click
        , class_ "main_button"
        ]
        [ text "Click Me" ]
    ]
