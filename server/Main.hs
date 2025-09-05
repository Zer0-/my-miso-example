{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

import System.Directory (getCurrentDirectory)
import Data.Proxy
import Servant.Server
    ( Server
    , serve
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
    , class_
    , src_
    )
import Miso.Html.Element (title_)
import qualified Servant
import Servant.API
import Miso.String (toMisoString)
import Servant.Miso.Html (HTML)
import Miso
    ( View
    , ToView (..)
    , MisoString
    )
import Data.Aeson (ToJSON)
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Data.Text.Lazy (toStrict)
import Data.Aeson.Text (encodeToLazyText)

import ApplicationTypes (Model, Action)

type ServerRoutes = Routes (Get '[HTML] (IndexPageData (View Model Action)))
data IndexPageData app = forall b. (ToJSON b, ToView Model app) => IndexPageData (b, app)

type RouteIndexPage a = a
type Routes a = RouteIndexPage a

type StaticRoute = "static" :> Servant.Raw

type API = StaticRoute :<|> ServerRoutes

instance ToHtml (IndexPageData a) where
    toHtml (IndexPageData (initial_data, app)) = toHtml
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
                , script_
                    [ class_ "initial-data"
                    , type_ "application/json"
                    ]
                    (toMisoString $ toStrict $ encodeToLazyText initial_data)

                , title_ [] [ "Chandlr" ]

                , js $ static_root <> "/init.js"
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

            js href =
                script_
                    [ type_ "module"
                    , src_ $ toMisoString href
                    ]
                    ""


server :: FilePath -> Wai.Application
server serve_static_dir_path =
    serve
        (Proxy @API)
        (staticHandler :<|> undefined)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer serve_static_dir_path


main :: IO ()
main = do
    putStrLn "Start!"
    cwd <- getCurrentDirectory

    let serve_static_dir_path = cwd <> "/static"

    Wai.run 8888 $ Wai.logStdout (server serve_static_dir_path)


-- TODO:
--  - Add code until we have warp serving a basic generated html page
--  - read data from local json file
--  - render out cat app using local data
--  - create client-side Main.hs that loads data from a header tag
--  - hydrate the app client-side
