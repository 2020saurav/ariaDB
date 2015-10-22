{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as T (fromStrict)
import qualified Data.ByteString.Lazy as BS (fromStrict)
import           Network.Wai (responseLBS, Application, pathInfo, requestMethod, requestBody)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types (status200, status404)
import           Network.HTTP.Types.Header (hContentType)

app :: Application
app req respond =
    do
        reqBody <- requestBody req
        respond $
            case requestMethod req of
                "GET" -> case pathInfo req of
                    [key] -> getValue key
                    _ -> notFound
                "POST" -> case pathInfo req of
                    [key] -> saveValue key reqBody
                    _ -> notFound
                "DELETE" -> case pathInfo req of
                    [key] -> deleteValue key
                    _ -> notFound

getValue key = responseLBS status200 [] value
    where value = encodeUtf8 . T.fromStrict $ key

saveValue key reqBody = responseLBS status200 [] k
    where
        k = encodeUtf8 . T.fromStrict $ key
        value = BS.fromStrict reqBody

deleteValue key = responseLBS status200 [] k
    where
        k = encodeUtf8 . T.fromStrict $ key

notFound = responseLBS status404 [] ""

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app
