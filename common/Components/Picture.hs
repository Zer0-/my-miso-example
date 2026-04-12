{-# LANGUAGE OverloadedStrings #-}

module Components.Picture where

import qualified Data.Vector as V
import Miso hiding (update, view, model)
import qualified Miso as M
import Miso.Html
    ( div_
    , img_
    )
import Miso.Html.Property
    ( src_
    , title_
    )

import qualified HttpClientTypes as Http

type PicturesInfo = V.Vector Http.PixabayImage

type Model = (PicturesInfo, Int)
data Action = ChangeInfo PicturesInfo

app :: PicturesInfo -> Int -> Component name Model Action
app ps i = (M.component (ps, i) update view) { M.logLevel = M.DebugAll }

update :: Action -> Effect parent Model Action
update (ChangeInfo newInfo) = modify $ \(_,i) -> (newInfo, i)

view :: Model -> View Model Action
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
