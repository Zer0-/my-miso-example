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
app pl = M.component initialModel update (view pl)

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
            , div_ [ class_ "collection-controls" ] [ mount_ CC.app ]
            ]
        , div_ [ class_ "pictures-list" ] [ mount_ pl ]
        ]
