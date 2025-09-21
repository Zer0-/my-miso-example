{-# LANGUAGE OverloadedStrings #-}

module MainApp where

import Miso
    ( component
    , App
    )
import Miso.Html
    ( h1_
    )

app :: App () ()
app = component () (const $ pure ()) $ const $
    h1_
        []
        [ "< Hello World >"
        , "Hello Everyone"
        ]
