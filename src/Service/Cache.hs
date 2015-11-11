{-|
Module      : Cache
Description : Caching library used by AriaDB
License     : BSD3
Maintainer  : abhi.iitk16@gmail.com
Stability   : experimental

This cache library exports 3 functions get, upsert and delete that is used by
AriaDB for faster reads. This library uses LRU cache library which handles all
these three requests in O(log n). Objects read are always cached. Objects
inserted in B+ Tree are cached by default. Objects deleted from B+ Tree are
also removed from the cache.
-}
module Cache (get, upsert, remove) where

import           Aria
import           Data.Cache.LRU as LRU
import           Data.IORef
import qualified BPlusTree.BPlusTree as BPTree

-- | 'get' takes an LRU cache and a key, and returns the corresponding value.
-- If the (key, value) is present in the cache, the value is returned
-- immediately otherwise value is looked up in the B+ Tree and the cache is
-- updated accordingly.
get :: IORef (LRU AriaKey AriaValue) -> AriaKey -> IO (Maybe AriaValue)
get lruCache key = do
    cacheValue <- readIORef lruCache
    let (newCache, val) = LRU.lookup key cacheValue
    case val of
        Just v -> do
            writeIORef lruCache newCache
            return (Just v)
        Nothing -> do
            val' <- BPTree.get key
            case val' of
                Just v -> do
                    let newCache = LRU.insert key v cacheValue
                    writeIORef lruCache newCache
                    return (Just v)
                Nothing -> return Nothing

-- | 'upsert' takes an LRU cache, a key and a value and inserts this key
-- value pair in B+ Tree if key was not present earlier, or updates its value
-- otherwise and updates the cache accordingly.
upsert :: IORef (LRU AriaKey AriaValue) -> AriaKey -> AriaValue -> IO ()
upsert lruCache key value = do
    cacheValue <- readIORef lruCache
    let newCache = LRU.insert key value cacheValue
    writeIORef lruCache newCache
    BPTree.upsert $ AriaKV key value

-- | 'remove' takes an LRU cache and a key. It removes the corresponding
-- (key, value) pair from B+ Tree and cache if the key was present.
remove :: IORef (LRU AriaKey AriaValue) -> AriaKey -> IO ()
remove lruCache key = do
    cacheValue <- readIORef lruCache
    let (newCache, _) = LRU.delete key cacheValue
    writeIORef lruCache newCache
    BPTree.remove key
