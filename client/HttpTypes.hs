module HttpTypes where

import Control.Concurrent.MVar (MVar)
import Miso (JSM)

data HttpMethod = GET | PUT | POST | DELETE | PATCH
    deriving Show

data HttpResult a
    = Error
    | HttpResponse
        { status_code :: Int
        , status_text :: String
        , body        :: Maybe a
        }

type HttpActionResult a = (JSM (), MVar (HttpResult a)) -- (abort, result)
