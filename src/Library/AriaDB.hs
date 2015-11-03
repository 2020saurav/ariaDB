module AriaDB (get, put, delete) where

import           Network.HTTP
import           Network.HTTP.Base
import           Network.URI
import           Types

hostURL :: String
hostURL = "http://localhost:3000/"

defaultURI :: URI
defaultURI = URI "http:" (Just ua) "" "" ""
    where ua = URIAuth "" "localhost" ":3000"

get :: (Read a) => AriaKey -> IO (Maybe a)
get key = do
    value <- getAux key
    case value of
        Just v  -> return . Just . read $ v
        Nothing -> return Nothing

getAux :: AriaKey -> IO (Maybe AriaValue)
getAux key = do
    let req = getRequest $ hostURL++key
    res  <- simpleHTTP req
    resCode <- getResponseCode res
    resBody <- getResponseBody res
    case resCode of
        (2,0,0) -> return (Just resBody)
        _       -> return Nothing -- 403 or 404

put :: (Show a) => AriaKey -> a -> IO ()
put key value = do
    let req = postRequestWithBody (hostURL++key) "" (show value)
    res <- simpleHTTP req
    return ()

delete :: AriaKey -> IO ()
delete key = do
    let req = deleteRequest key
    res <- simpleHTTP req
    return ()

deleteRequest :: AriaKey -> Request String
deleteRequest key = Request uri reqMethod [] ""
    where uri       = createURIWithKey key
          reqMethod = DELETE

createURIWithKey :: AriaKey -> URI
createURIWithKey key = case uri of
    Just u  -> u
    Nothing -> defaultURI
    where uri = parseURI $ hostURL++key
