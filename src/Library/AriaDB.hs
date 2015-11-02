module AriaDB where
-- export get, put and delete

import           Network.HTTP
import           Types

hostURL :: String
hostURL = "http://localhost:3000/"

get :: (Read a) => AriaKey -> IO a
get key = getAux key >>= \x -> return $ read x

getAux :: AriaKey -> IO AriaValue
getAux key = do
    let req = getRequest $ hostURL++key
    res  <- simpleHTTP req
    getResponseBody res

put :: (Show a) => AriaKey -> a -> IO ()
put key value = do
    let req = postRequestWithBody (hostURL++key) "" (show value)
    res <- simpleHTTP req
    return ()
