{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Components.CollectionControls where

import Control.Monad (when)
import Miso hiding (update, view, model)
import Miso.Html
    ( h4_
    , onInput
    , onChange
    , input_
    , div_
    , span_
    )
import Miso.Html.Property
    ( class_
    , autofocus_
    , value_
    , max_
    , min_
    , type_
    , tabindex_
    )
import qualified Miso as M
import Miso.JSON (ToJSON, FromJSON)

type CollectionControls parent = Component parent Model Action

newtype OutMessage = CountChanged Int deriving (ToJSON, FromJSON)

collectionControlsOutTopic :: Topic OutMessage
collectionControlsOutTopic = topic "collection-controls-out"

data Model = Model
    { count :: Int
    }
    deriving (Show, Eq)

data Action = ChangeCount Int

initialModel :: Model
initialModel = Model 6

app :: CollectionControls parent
app = M.component initialModel update view

update :: Action -> Effect parent Model Action
update (ChangeCount i) = do
    m <- get
    let old_value = count m

    io_ $ do
        consoleLog $ ("previous value: " <> (toMisoString $ old_value))
        consoleLog $ ("update " <> (toMisoString $ show i))
        publish collectionControlsOutTopic $ CountChanged i

    when (old_value /= i) $
        modify (\model -> model { count = i })

readString :: (Read a) => MisoString -> a
readString = read . fromMisoString


readNum :: MisoString -> Int
readNum "" = 0
readNum x = readString x


view :: Model -> View Model Action
view model =
    div_
      [ class_ "controls" ]
      [ div_
          [ class_ "controls--pic_count" ]
          [ span_ [] [ "Picture count: " ]
          , input_
            [ type_ "number"
            , min_ "0"
            , max_ "20"
            , value_ (toMisoString $ count model)
            , autofocus_ True
            , tabindex_ $ toMisoString (1 :: Int)

            , onInput $ ChangeCount . readNum
            , onChange $ ChangeCount . readNum
            ]
          ]
      , h4_ [ class_ "controls--summary" ] [ text $ toMisoString $ count model ]
      ]
