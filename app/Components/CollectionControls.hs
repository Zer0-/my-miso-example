{-# LANGUAGE OverloadedStrings #-}

module Components.CollectionControls where

import Control.Monad (when)
import Control.Monad.State (modify, get)
import Miso hiding (update, view, model)
import Miso.String hiding (count)
import qualified Miso as M

import qualified Components.PicturesList as PL

data Model = Model
    { count :: Int
    , topic :: MisoString
    }
    deriving (Show, Eq)

data Action
    = ChangeCount Int
    | ChangeTopic MisoString
    | SubmitTopic

initialModel :: Model
initialModel = Model 0 "Kitty Cats"

app :: PL.PicturesListComponent -> App Model Action
app pl = M.App
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

    io $ do
        consoleLog $ ("previous value: " <> (toMisoString $ old_value))
        consoleLog $ ("update " <> (toMisoString $ show i))
        notify pl $ PL.ChangeCount i

    when (old_value /= i) $
        modify (\model -> model { count = i })

update _ (ChangeTopic t) = do
    modify (\model -> model { topic = t })


update pl SubmitTopic = do
    model <- get

    io $ notify pl $ PL.ChangeTopic $ topic model


readString :: (Read a) => MisoString -> a
readString = read . fromMisoString


readNum :: MisoString -> Int
readNum "" = 0
readNum x = readString x


view :: Model -> View Action
view model =
    div_
      []
      [ div_
          []
          [ span_ [] [ "Picture count: " ]
          , input_
            [ type_ "number"
            , min_ "0"
            , max_ "20"
            , value_ (toMisoString $ count model)

            , onInput $ ChangeCount . readNum
            , onChange $ ChangeCount . readNum
            ]
          ]
      , div_
          []
          [ span_ [] [ "Keyword: " ]
          , input_
            [ type_ "text"
            , value_ $ topic model
            , onInput ChangeTopic
            , onChange ChangeTopic
            ]
          ]
          , input_
                [ type_ "button"
                , value_ "UpdateKeyword"

                , onClick SubmitTopic
                ]
      , h1_ [] [ text $ toMisoString $ count model ]
      ]
