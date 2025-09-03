{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Components.CollectionControls where

import Control.Monad (when)
import Miso hiding (update, view, model)
import Miso.String hiding (count)
import Miso.Svg (tabindex_)
import qualified Miso as M

import qualified Components.PicturesList as PL

type CollectionControls = Component "collection-controls" Model Action

data Model = Model
    { count :: Int
    , topic :: MisoString
    }
    deriving (Show, Eq)

data Action
    = ChangeCount Int
    | InputTopic MisoString
    | ChangeTopic MisoString
    | SubmitTopic

initialModel :: Model
initialModel = Model 6 "Kitty Cats"

app :: PL.PicturesListComponent -> CollectionControls
app pl = M.Component
    { M.model = initialModel
    , M.update = update pl
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    }


update :: PL.PicturesListComponent -> Action -> Effect Model Action
update pl (ChangeCount i) = do
    m <- get
    let old_value = count m

    io_ $ do
        consoleLog $ ("previous value: " <> (toMisoString $ old_value))
        consoleLog $ ("update " <> (toMisoString $ show i))
        notify pl $ PL.ChangeCount i

    when (old_value /= i) $
        modify (\model -> model { count = i })

update _ (InputTopic t) = do
    modify (\model -> model { topic = t })

update _ (ChangeTopic t) = do
    issue $ InputTopic t
    issue SubmitTopic

update pl SubmitTopic = do
    model <- get

    io_ $ notify pl $ PL.ChangeTopic $ topic model


readString :: (Read a) => MisoString -> a
readString = read . fromMisoString


readNum :: MisoString -> Int
readNum "" = 0
readNum x = readString x


view :: Model -> View Action
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
      , form
          [ class_ "controls--topic"

          , onSubmit SubmitTopic
          ]
          [ span_ [] [ "Topic: " ]
          , input_
            [ type_ "text"
            , value_ $ topic model
            , tabindex_ $ toMisoString (2 :: Int)

            , onInput InputTopic
            , onChange InputTopic
            ]
          , input_
            [ type_ "submit"
            , value_ "Update Topic"
            , tabindex_ $ toMisoString (3 :: Int)

            , onClick SubmitTopic
            ]
          ]
      , h4_ [ class_ "controls--summary" ] [ text $ toMisoString $ count model ]
      ]
