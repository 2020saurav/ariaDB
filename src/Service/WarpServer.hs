{-# LANGUAGE OverloadedStrings #-}

import           Aria
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.Wai (responseLBS, Application, pathInfo, requestMethod, requestBody)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types (status200, status404)
import           Network.HTTP.Types.Header (hContentType)
import qualified BPlusTree.BPlusTree as BPTree

app :: Application
app req respond =
    case requestMethod req of
        "GET" -> case pathInfo req of
            [key] -> do
                value <- BPTree.get $ textToAriaKey key
                case value of
                    Just v  -> respond $ responseLBS status200 [] $ ariaToText v
                    Nothing -> respond notFound
            _ -> respond notFound

        "POST" -> case pathInfo req of
            [key] -> do
                reqBody <- requestBody req
                BPTree.upsert $ AriaKV (textToAriaKey key) (textToAriaValue reqBody)
                respond $ responseLBS status200 [] ""

        "DELETE" -> case pathInfo req of
            [key] -> do
                BPTree.remove (textToAriaKey key)
                respond $ responseLBS status200 [] ""

notFound = responseLBS status404 [] "Not Found"

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app
