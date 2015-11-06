{-|
Module      : AriaDB
Description : Client side library to provide interface to AriaDB service
License     : BSD3
Maintainer  : 2020saurav@gmail.com
Stability   : experimental

This module is a client side library for Haskell to provide an interface to
AriaDB service. It interacts with the service using HTTP REST calls and exposes
simple 'get', 'put', and 'delete' methods to use in Haskell codes. In order to
use the library, the data type of the value must derive 'Show' and 'Read'
classes.
-}
module AriaDB (get, put, delete) where
import           Network.HTTP
import           Network.HTTP.Base
import           Network.URI
import           Types

-- | URL of the service to connect to
hostURL :: String
hostURL = "http://localhost:3000/"

-- | default URI of the service
defaultURI :: URI
defaultURI = URI "http:" (Just ua) "" "" ""
    where ua = URIAuth "" "localhost" ":3000"

-- | 'get' method takes in a key of type 'AriaKey' and returns a 'Maybe' value
-- inside an IO Monad, after fetching it from AriaDB server. It can be used as
-- follows:
--
-- @
--     import 'AriaDB'
--
--     data Person = Person {
--         firstName :: String,
--         lastName  :: String
--     } deriving (Show, Read, Eq)
--
--     foo = Person \"John\" \"Doe\"
--
--     main = do
--         let testKey = \"k1\"
--         'put' testKey foo
--
--         bar <- 'get' testKey
--         case bar of
--             Just v  -> print (v::Person)      -- Type Assigned
--             Nothing -> print \"Nothing\"
--
--         baz <- 'get' testKey
--         case baz of
--             Just v  -> print $ v == foo       -- Type Inferred
--             Nothing -> print "Value not found"
--
--         'delete' testKey
--         qux <- 'get' testKey
--         case qux of
--             Just v  -> print (v::Person)      -- Type Assigned
--             Nothing -> print \"Nothing\"
-- @
get :: (Read a) => AriaKey -> IO (Maybe a)
get key = do
    value <- getAux key
    case value of
        Just v  -> return . Just . read $ v
        Nothing -> return Nothing

-- | An auxilliary function for 'get' method
getAux :: AriaKey -> IO (Maybe AriaValue)
getAux key = do
    let req = getRequest $ hostURL++key
    res  <- simpleHTTP req
    resCode <- getResponseCode res
    resBody <- getResponseBody res
    case resCode of
        (2,0,0) -> return (Just resBody)
        _       -> return Nothing -- 403 or 404

-- | 'put' method takes a key and a value, and sends it to AriaDB server to
-- upsert the value corresponding to the key. Example of 'put' in given under
-- documentation for 'get'
put :: (Show a) => AriaKey -> a -> IO ()
put key value = do
    let req = postRequestWithBody (hostURL++key) "" (show value)
    res <- simpleHTTP req
    return ()

-- | 'delete' method takes a key, and sends it to AriaDB server for deletion of
-- corresponding value.
delete :: AriaKey -> IO ()
delete key = do
    let req = deleteRequest key
    res <- simpleHTTP req
    return ()

-- | It creates a 'Request' object for deletion of the given key.
deleteRequest :: AriaKey -> Request String
deleteRequest key = Request uri reqMethod [] ""
    where uri       = createURIWithKey key
          reqMethod = DELETE

-- | It creates a URI with the given key
createURIWithKey :: AriaKey -> URI
createURIWithKey key = case uri of
    Just u  -> u
    Nothing -> defaultURI
    where uri = parseURI $ hostURL++key
