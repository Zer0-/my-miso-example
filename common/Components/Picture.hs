{-# LANGUAGE OverloadedStrings #-}

module Components.Picture where

import qualified Data.Vector as V
import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.String (toMisoString)

import qualified HttpClientTypes as Http

type PicturesInfo = V.Vector Http.PixabayImage

type Model = (PicturesInfo, Int)
data Action = ChangeInfo PicturesInfo

app :: PicturesInfo -> Int -> Component name Model Action
app ps i = M.Component
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
update (ChangeInfo newInfo) = modify $ \(_,i) -> (newInfo, i)

view :: Model -> View Action
view (ps, i) =
    div_
        []
        [ img_
            [ src_ $ Http.webformatURL picInfo
            , title_  $ "Image-" <> toMisoString i
            ]
        ]

    where
        picInfo = (V.!) ps i
