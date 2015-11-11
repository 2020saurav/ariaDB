{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main (WarpServer)
Description : HTTP (Wai-Warp) Server as end point for APIs of AriaDB
License     : BSD3
Maintainer  : 2020saurav@gmail.com
Stability   : experimental

WarpServer is a HTTP server which serves as the end point for APIs of AriaDB
service.
-}
module Main (main) where

import           Aria
import           Cache
import           Data.Cache.LRU
import           Data.IORef
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.Wai (responseLBS,
                            Application, pathInfo, requestMethod, requestBody)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types (status200, status403, status404)
import           Network.HTTP.Types.Header (hContentType)

-- | It takes an LRU cache and returns a Wai application to handle requests and responses
app' :: IORef (LRU AriaKey AriaValue) -> Application
app' lruCache = app where
    app req respond =
        case requestMethod req of
            "GET" -> case pathInfo req of
                [key] -> do
                    value <- Cache.get lruCache (textToAriaKey key)
                    case value of
                        Just v  -> respond $ responseLBS status200 [] $ ariaToText v
                        Nothing -> respond notFound
                _ -> respond forbidden

            "POST" -> case pathInfo req of
                [key] -> do
                    reqBody <- requestBody req
                    Cache.upsert lruCache (textToAriaKey key) (textToAriaValue reqBody)
                    respond $ responseLBS status200 [] ""
                _ -> respond forbidden

            "DELETE" -> case pathInfo req of
                [key] -> do
                    Cache.remove lruCache (textToAriaKey key)
                    respond $ responseLBS status200 [] ""
                _ -> respond forbidden
            _ -> respond forbidden

-- | Response 404
notFound  = responseLBS status404 [] "Not Found"

-- | Response 403
forbidden = responseLBS status403 [] "Forbidden"

-- | A journey of thousand miles must begin with a single step. This is that step
main = do
    let port = 3000
    lruCache <- newIORef (newLRU Cache.lruCacheSize)
    putStrLn $ "Listening on port " ++ show port
    run port (app' lruCache)
