{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Components.MainComponent where

import Miso hiding (update, view, model)
import qualified Miso as M
import Data.Proxy
import Servant.API

import Components.Types
import Routes

initialModel :: Model
initialModel = M.URI "" Nothing "" "" ""

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
{-
update Clicked =
    io_ $ do
        consoleLog "Button Clicked"
        uri <- getURI
        consoleLog $ toMisoString $ show uri
        let new_u = new_uri uri
        consoleLog $ toMisoString $ show new_u
        -- pushURI new_u

    where
        new_uri u = u { M.uriPath = "clicked" }
        

update (URIChanged new_u) = do
    io_ $ consoleLog "URI Changed"
    modify $ const new_u
-}

update Clicked = modify (\m -> m { M.uriPath = "clicked" })

view :: Model -> View Action
view model = either (const page404) id $
    route (Proxy :: Proxy Route) handlers id model

    where
        handlers
            =    (const $ component_ homeApp [])
            :<|> clicked

        clicked = const $ div_
            [ class_ "topmatter" ]
            [ h1_ [ class_ "title" ] [ "Clicked!" ]
            , p_ [ class_ "subtitle" ] [ "You just lost the game." ]
            ]

page404 :: View Action
page404 = h1_ [] [ text "404 Not Found" ]


homeApp :: Component "home" () ()
homeApp = M.Component
    { M.model = ()
    , M.update = updateHome
    , M.view = const home
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
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
updateHome _ = io_ $ notify app Clicked
