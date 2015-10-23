{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as T (fromStrict)
import qualified Data.ByteString.Lazy as BS (fromStrict)
import           Network.Wai (responseLBS, Application, pathInfo, requestMethod, requestBody)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types (status200, status404)
import           Network.HTTP.Types.Header (hContentType)

textToAriaValue = BS.fromStrict
textToAriaKey   = encodeUtf8 . T.fromStrict

app :: Application
app req respond =
    do
        reqBody <- requestBody req
        respond $
            case requestMethod req of
                "GET" -> case pathInfo req of
                    [key] -> handleGetRequest (textToAriaKey key)
                    _ -> notFound
                "POST" -> case pathInfo req of
                    [key] -> handlePostRequest (textToAriaKey key) (textToAriaValue reqBody)
                    _ -> notFound
                "DELETE" -> case pathInfo req of
                    [key] -> handleDeleteRequest (textToAriaKey key)
                    _ -> notFound

handleGetRequest key        = responseLBS status200 [] $ getValue key
handlePostRequest key value = responseLBS status200 [] $ saveValue key value
handleDeleteRequest key     = responseLBS status200 [] $ deleteValue key
notFound                    = responseLBS status404 [] $ ""

getValue key        = key
saveValue key value = key
deleteValue key     = key

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app
