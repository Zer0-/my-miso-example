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
    , class_
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
    , MisoString
    , mount_
    )
import Miso.JSON (ToJSON, decode, encode)
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Data.Text.IO as T
import System.Exit (exitFailure)
import qualified Data.Vector as V

import ApplicationTypes (Model, Action)
import HttpClientTypes (PixabayResponse, hits)
import qualified Components.MainComponent as Main
import qualified Components.PicturesList as PL

type ServerRoutes = Routes (Get '[HTML] IndexPageData)

data IndexPageData = forall b. (ToJSON b) => IndexPageData (b, App Model Action)

type RouteIndexPage a = a
type Routes a = RouteIndexPage a

type StaticRoute = "static" :> Servant.Raw

type API = StaticRoute :<|> ServerRoutes

instance ToHtml IndexPageData where
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
                    (encode initial_data)

                , title_ [] [ "My Miso Example" ]

                , js_wasm $ static_root <> "/init.js"
                -- , js_js $ static_root <> "/all.js" -- Uncomment this and comment out the previous line to load the javascript version (TODO: make this a commandline flag or something)
                , css $ static_root <> "/style.css"
                ]
            , body_ [] [ mount_ (app :: App Model Action) ]
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
                    , defer_ True
                    ]
                    ""


server :: FilePath -> PixabayResponse -> Wai.Application
server serve_static_dir_path sample_response =
    serve
        (Proxy @API)
        (staticHandler :<|> mainView sample_response)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer serve_static_dir_path


mainView :: PixabayResponse -> Handler IndexPageData
mainView sample_response = pure $
    IndexPageData (sample_response, Main.app pl)

    where
        pl :: PL.PicturesListComponent Model
        pl = PL.app pl_model

        pl_model :: PL.Model
        pl_model = PL.initialModel
            { PL.pictureInfo = V.fromList (hits sample_response) }


readSampleResponseFromFile :: FilePath -> IO PixabayResponse
readSampleResponseFromFile cwd = do
    let filePath = cwd <> "/static/sample_response_local.json"
    content <- T.readFile filePath
    case decode content :: Maybe PixabayResponse of
        Nothing -> do
            putStrLn "Error: Invalid JSON format."
            exitFailure
        Just response -> return response


main :: IO ()
main = do
    putStrLn "Start!"
    cwd <- getCurrentDirectory

    let serve_static_dir_path = cwd <> "/static"

    sample_response <- readSampleResponseFromFile cwd

    putStrLn "Beginning to listen on 8888"

    Wai.run 8888 $ Wai.logStdout (server serve_static_dir_path sample_response)


-- TODO:
--  - Add code until we have warp serving a basic generated html page
--  - read data from local json file
--  - render out cat app using local data
--  - create client-side Main.hs that loads data from a header tag
--  - hydrate the app client-side
