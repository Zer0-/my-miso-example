{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Components.MainComponent where

import Miso hiding (update, view, model)
import qualified Miso as M
import Data.Proxy
import Servant.API
import Data.Map (singleton)

import Components.Types
import Routes

initialModel :: Model
initialModel = M.URI "" Nothing "" "" ""


app :: Component Model Action
app = M.Component
    { M.model = initialModel
    , M.update = update
    , M.view = view
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
update Initialize = do
    subscribe clickTopic Clicked
    io_ $ consoleLog "1"
    io_ $ consoleLog "2"
update (Clicked _) = modify (\m -> m { M.uriPath = "clicked" })


view :: Model -> View Action
view model = either (const page404) id $
    route (Proxy :: Proxy Route) handlers id model

    where
        handlers
            =    (const $ component_ homeApp)
            :<|> clicked

        clicked = const $ div_
            [ class_ "topmatter" ]
            [ h1_ [ class_ "title" ] [ "Clicked!" ]
            , p_ [ class_ "subtitle" ] [ "You just lost the game." ]
            ]


page404 :: View Action
page404 = h1_ [] [ text "404 Not Found" ]


homeApp :: Component () HomeAction
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


data HomeAction = Mounted | Unmounted | Click

home :: View HomeAction
home = div_
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


updateHome :: HomeAction -> Effect () HomeAction
updateHome Click = publish clickTopic ()
updateHome Mounted = io_ $ consoleLog "home Mounted"
updateHome Unmounted = io_ $ consoleLog "home Unmounted"
