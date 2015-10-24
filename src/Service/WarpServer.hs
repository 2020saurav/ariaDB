{-# LANGUAGE OverloadedStrings #-}

import           Aria
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.Wai (responseLBS, Application, pathInfo, requestMethod, requestBody)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types (status200, status404, status406)
import           Network.HTTP.Types.Header (hContentType)
import qualified UnaryTree as UTree

app :: Application
app req respond =
    case requestMethod req of
        "GET" -> case pathInfo req of
            [key] -> do
                value <- UTree.find $ textToAriaKey key
                case value of
                    Just v  -> respond $ responseLBS status200 [] $ ariaToText v
                    Nothing -> respond notFound
            _ -> respond notFound

        "POST" -> case pathInfo req of
            [key] -> do
                reqBody   <- requestBody req
                insertKey <- UTree.insert (textToAriaKey key) (textToAriaValue reqBody)
                case insertKey of
                    Left err -> respond $ responseLBS status406 [] $ ariaToText "Key Exists"
                    Right k  -> respond $ responseLBS status200 [] $ ariaToText k
            _ -> respond notFound

        "DELETE" -> case pathInfo req of
            [key] -> respond $ responseLBS status200 [] $ ariaToText $ textToAriaKey key
            _ -> respond notFound

notFound = responseLBS status404 [] "Not Found"

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

-- Testing
-- GET : curl http://localhost:3000/102
-- POST : curl -X POST --data "Piyush" http://localhost:3000/103
-- DELETE : will see how to handle these directly in B+ Tree index.
