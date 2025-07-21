{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Components.MainComponent where

import Miso hiding (update, view, model)
import qualified Miso as M
import Data.Proxy
import Servant.API hiding (addHeader)
import Data.Map (singleton, empty)

import Components.Types
import Routes

initialModel :: Model
initialModel = M.URI "" Nothing "" "" ""


app :: Component Model Action
app = M.Component
    { M.model = initialModel
    , M.update = update
    , M.view = const $ addAlwaysComponent $ component_ [] homeApp
    -- , M.view = const $ addAlwaysComponent clicked
    , M.subs = []
    , M.events = mempty
    , M.styles = []
    , M.initialAction = Just Initialize
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    }


update :: Action -> Effect Model Action
update Initialize = subscribe clickTopic Clicked
update (Clicked _) = modify (\m -> m { M.uriPath = "clicked" })
update HeaderMounted = io_ $ consoleLog "Header Mounted!"
update HeaderUnmounted = io_ $ consoleLog "Header Unmounted!"


view :: Model -> View Action
view model = either (const page404) addAlwaysComponent $
    route (Proxy :: Proxy Route) handlers id model

    where
        handlers
            =    (const $ component_ [] homeApp)
            :<|> (const clicked)


clicked :: View b
clicked = div_
    [ class_ "topmatter" ]
    [ h1_ [ class_ "title" ] [ "Clicked!" ]
    , p_ [ class_ "subtitle" ] [ "You just lost the game." ]
    ]


addAlwaysComponent :: View Action -> View Action
addAlwaysComponent = addToView $ component_
    [ onMounted HeaderMounted
    , onUnmounted HeaderUnmounted
    ]
    alwaysApp


addClicked :: View a -> View a
addClicked = addToView clicked


page404 :: View Action
page404 = h1_ [] [ text "404 Not Found" ]


homeApp :: Component () ()
homeApp = M.Component
    { M.model = ()
    , M.update = updateHome
    , M.view = const home
    , M.subs = []
    , M.events = singleton "click" False
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    }


home :: View ()
home = div_
    [ class_ "topmatter" ]
    [ h1_ [ class_ "title" ] [ "Bug Demo" ]
    , p_ [ class_ "subtitle" ] [ "Bugs are bad." ]
    , button_
        [ onClick ()
        , class_ "main_button"
        ]
        [ text "Click Me" ]
    ]


updateHome :: () -> Effect () ()
updateHome _ = publish clickTopic ()


addToView :: View action -> View action -> View action
addToView child (VNode a b cs ds) = VNode a b cs (child : ds)
addToView _ v = v

alwaysApp :: Component () ()
alwaysApp = M.Component
    { M.model = ()
    , M.update = const $ return ()
    , M.view = const alwaysView
    , M.subs = []
    , M.events = empty
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    }


alwaysView :: View ()
alwaysView = div_ [ class_ "title" ] [ "Top header should always be here" ]
