{-# LANGUAGE OverloadedStrings #-}

import           Aria
import           Data.Text.Lazy.Encoding (encodeUtf8)
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
                    [key] -> handleGetRequest (textToAriaKey key)
                    _ -> notFound
                "POST" -> case pathInfo req of
                    [key] -> handlePostRequest (textToAriaKey key) (textToAriaValue reqBody)
                    _ -> notFound
                "DELETE" -> case pathInfo req of
                    [key] -> handleDeleteRequest (textToAriaKey key)
                    _ -> notFound

handleGetRequest key        = responseLBS status200 [] $ getValue key dummyDB
handlePostRequest key value = responseLBS status200 [] $ saveValue key value dummyDB
handleDeleteRequest key     = responseLBS status200 [] $ deleteValue key dummyDB
notFound                    = responseLBS status404 [] $ ""

-- TODO : Following functions to be modified to search in B+ Tree
getValue key db = case db of
                    kv:kvs  -> if ariaKey kv == key
                                then ariaToText . ariaValue $ kv
                                else getValue key kvs
                    _       -> ariaToText "Nothing Found"

-- TODO : needless to write these functions now. To be handled in B+ Tree insertion
saveValue key value db = ariaToText "Foo"

-- TODO : B+ Tree deletion
deleteValue key db     = ariaToText "Foo"

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

-- Testing
dummyDB = [AriaKV "100" "Abhilak", AriaKV "101" "Proneet", AriaKV "102" "Saurav"]
-- GET : curl http://localhost:3000/102
-- POST : curl -X POST --data "Piyush" http://localhost:3000/103 : not implemented right now
-- DELETE : will see how to handle these directly in B+ Tree index.
