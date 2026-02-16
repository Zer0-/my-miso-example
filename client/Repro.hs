----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import           Miso.DSL
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
#ifdef INTERACTIVE
main = reload (startApp defaultEvents app)
#else
main = miso defaultEvents $ \_ -> app
#endif
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Int Action
app = (component 0 updateModel viewModel)
  { logLevel = DebugHydrate
  , hydrateModel = Just $ do
      consoleLog "inside debug hydrate"
      x <- fromJSValUnchecked =<< jsg "globalThis" ! "model" ! "number"
      consoleLog ("got eem: " <> ms x)
      pure x
  }
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent Int Action
updateModel = \case
  AddOne -> this += 1
  SubtractOne -> this -= 1
  SayHelloWorld -> io_ (consoleLog "Hello world")
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Int -> View Int Action
viewModel x =
  H.div_
    [ P.className "counter"
    ]
    [ H.button_ [ H.onClick AddOne ] [ text "+" ]
    , text (ms x)
    , H.button_ [ H.onClick SubtractOne ] [ text "-" ]
    , H.br_ []
    , H.button_ [ H.onClick SayHelloWorld ] [ text "Alert Hello World!" ]
    ]
----------------------------------------------------------------------------
