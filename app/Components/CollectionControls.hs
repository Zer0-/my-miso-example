{-# LANGUAGE OverloadedStrings #-}

module Components.CollectionControls where

import Control.Monad (when)
import Control.Monad.State (modify, get)
import Miso hiding (update, view, model)
import Miso.String hiding (count)
import qualified Miso as M

data Model = Model
    { count :: Int
    }
    deriving (Show, Eq)

data Action = ChangeCount Int

initialModel :: Model
initialModel = Model 0

app :: App Model Action
app = M.App
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
update (ChangeCount i) = do
    m <- get
    let old_value = count m

    io $ do
        consoleLog $ ("previous value: " <> (toMisoString $ old_value))
        consoleLog $ ("update " <> (toMisoString $ show i))

    when (old_value /= i) $
        modify (\model -> model { count = i })


readString :: (Read a) => MisoString -> a
readString = read . fromMisoString


view :: Model -> View Action
view model =
    div_
      []
      [ div_
          []
          [ span_ [] [ "Picture count: " ]
          , input_
            [ type_ "number"
            , min_ "1"
            , value_ (toMisoString $ count model)
            , onInput $ ChangeCount . readString
            , onChange $ ChangeCount . readString
            ]
          ]
      , div_
          []
          [ span_ [] [ "Keyword: " ]
          , input_
            [ type_ "text"
            ]
          ]
      , h1_ [] [ text $ toMisoString $ count model ]
      ]
