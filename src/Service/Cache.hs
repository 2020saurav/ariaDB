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
module Cache (get, upsert, remove, lruCacheSize) where

import           Aria
import qualified BPlusTree.BPlusTree as BPTree
import           BPlusTree.Leaf as Leaf
import           BPlusTree.Types
import           Data.Cache.LRU as LRU
import           Data.IORef

-- | Maximum size of LRU cache. The LRU cache is guaranteed to not grow above
-- the specified number of entries.
lruCacheSize :: Maybe Integer
lruCacheSize = Just 10000

-- | 'get' takes an LRU cache and a key, and returns the corresponding value.
-- If the (key, value) is present in the cache, the value is returned
-- immediately otherwise value is looked up in the B+ Tree and the cache is
-- updated with complete leaf key-values to boost up sequential reads.
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
                    leafName <- BPTree.findLeaf key
                    bulkload lruCache leafName
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

-- | It loads up entire key-value pairs in the leaf into the cache. The leaf
-- values are prepended to the existing ones. LRU cache library achieves it in
-- O(n*logn). Loading entire leaf will greatly enhance the sequential reads.
bulkload :: IORef (LRU AriaKey AriaValue) -> BPTFileName -> IO ()
bulkload lruCache leafName = do
    leaf <- Leaf.readLeaf leafName
    cacheValue <- readIORef lruCache
    let oldCacheValues = LRU.toList cacheValue
    let newCacheValues = getKVList leaf
    let newCache = LRU.fromList lruCacheSize (newCacheValues ++ oldCacheValues)
    writeIORef lruCache newCache

-- | Essentially, it drops those key-value pairs whose values are 'Nothing'.
-- Those keys are deleted keys and need not be added in the cache.
getKVList :: Leaf -> [(AriaKey, AriaValue)]
getKVList leaf = map getKV filteredList where
    filteredList = filter isSndNonEmpty zippedList where
        zippedList = zip (Leaf.keys leaf) (Leaf.values leaf)

-- | This function is to assist 'getKVList' to map over the list to remove those
-- key-value whose value is 'Nothing'.
getKV :: (AriaKey, Maybe AriaValue) -> (AriaKey, AriaValue)
getKV kv = case snd kv of
    Just x  -> (fst kv, x )
    Nothing -> (fst kv, "") -- this case won't arise.

-- | This function checks if the second entry in the pair is 'Nothing'. This is
-- to filter the zipped list in 'getKVList'
isSndNonEmpty :: (a, Maybe b) -> Bool
isSndNonEmpty x = case snd x of
    Just _  -> True
    Nothing -> False
