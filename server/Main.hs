{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

import System.Directory (getCurrentDirectory)
import Data.Proxy
import Servant.Server
    ( Server
    , serve
    , Handler
    )
import qualified Network.Wai as Wai
import Miso.Html
    ( ToHtml (..)
    , doctype_
    , html_
    , head_
    , meta_
    , link_
    , body_
    , script_
    )
import Miso.Html.Property
    ( charset_
    , name_
    , content_
    , rel_
    , href_
    , type_
    , src_
    , language_
    , defer_
    )
import Miso.Html.Element (title_)
import qualified Servant
import Servant.API
import Miso.String (toMisoString)
import Servant.Miso.Html (HTML)
import Miso
    ( App
    , ToView (..)
    , MisoString
    )
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

import qualified MainApp as Main

type Model = ()
type Action = ()

type ServerRoutes = Routes (Get '[HTML] (IndexPageData (App Model Action)))

data IndexPageData app = ToView Model app => IndexPageData app

type RouteIndexPage a = a
type Routes a = RouteIndexPage a

type StaticRoute = "static" :> Servant.Raw

type API = StaticRoute :<|> ServerRoutes

instance ToHtml (IndexPageData a) where
    toHtml (IndexPageData app) = toHtml
        [ doctype_
        , html_
            []
            [ head_
                []
                [ meta_ [ charset_ "utf-8" ]
                , meta_
                    [ name_ "viewport"
                    , content_ "width=device-width, initial-scale=1.0"
                    ]

                , title_ [] [ "Chandlr" ]

                -- , js_wasm $ static_root <> "/init.js"
                -- , js_js $ static_root <> "/all.js" -- Uncomment this and comment out the previous line to load the javascript version (TODO: make this a commandline flag or something)
                , css $ static_root <> "/style.css"
                ]
            , body_ [] [ toView @Model app ]
            ]
        ]

        where
            static_root :: MisoString
            static_root = "/static"

            css href =
                link_
                    [ rel_ "stylesheet"
                    , type_ "text/css"
                    , href_ $ toMisoString href
                    ]

            js_wasm href =
                script_
                    [ type_ "module"
                    , src_ $ toMisoString href
                    ]
                    ""

            js_js href =
                script_
                    [ language_ "javascript"
                    , src_ $ toMisoString href
                    , defer_ "true"
                    ]
                    ""


server :: FilePath -> Wai.Application
server serve_static_dir_path =
    serve
        (Proxy @API)
        (staticHandler :<|> mainView)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer serve_static_dir_path


mainView :: Handler (IndexPageData (App Model Action))
mainView = pure $
    IndexPageData Main.app


main :: IO ()
main = do
    putStrLn "Start!"
    cwd <- getCurrentDirectory

    let serve_static_dir_path = cwd <> "/static"

    putStrLn "Beginning to listen on 8888"

    Wai.run 8888 $ Wai.logStdout (server serve_static_dir_path)
