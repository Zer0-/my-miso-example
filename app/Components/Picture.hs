{-# LANGUAGE OverloadedStrings #-}

module Components.Picture where

import qualified Data.Vector as V
import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.String (toMisoString)

import qualified HttpClientTypes as Http

type PicturesInfo = V.Vector Http.PixabayImage

type Model = (PicturesInfo, Int)
type Action = ()

app :: PicturesInfo -> Int -> App Model Action
app ps i = M.App
    { M.model = (ps, i)
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
update _ = io $ return ()

view :: Model -> View Action
view (ps, i) =
    div_
        [ class_ "picture" ]
        [ h1_ [] [ text $ "Image" <> toMisoString i ]
        , img_ [ src_ $ Http.webformatURL picInfo ]
        ]

    where
        picInfo = (V.!) ps i
