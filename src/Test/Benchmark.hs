import           Aria
import qualified BPlusTree.BPlusTree as BPTree
import           Cache
import           Control.Monad
import           Data.Cache.LRU
import           Data.IORef
import           Data.List
import           Data.Time.Clock.POSIX
import           System.Random
import           System.Random.Shuffle

createAriaKV :: Int -> AriaKV
createAriaKV k = AriaKV (show k) (show k)

createAriaKey :: Int -> AriaKey
createAriaKey = show

createAriaValue :: Int -> Maybe AriaValue
createAriaValue k = Just (show k)

checkListIO :: (Eq a) => [IO a] -> [a] -> IO Bool
checkListIO (iox:ioxs) (y:ys) = do
    x <- iox
    if x == y then (checkListIO ioxs ys) >>= \x -> return (x && True)
    else return False
checkListIO _ _ = return True

main = do
    -- WARNING : Comment non-relevant parts before running.
    -- -- insert
    let xs = take 10000000 [1..]
    z <- shuffleM xs
    let ys = take 100 z
    startTime <- getPOSIXTime
    let ariaKVList = map createAriaKV ys
    mapM_ BPTree.upsert ariaKVList
    endTime <- getPOSIXTime
    print (endTime - startTime)

    -- Random Repeated Data
    -- from 1..n, select m=50 keys randomly. Take each k=20 times.
    let xs = take 100000 [1..]
    z <- shuffleM xs
    let y = take 50 z
    let y2 = y ++ y
    let y4 = y2 ++ y2
    let y8 = y4 ++ y4
    let y16 = y8 ++ y8
    let y20 = y16 ++ y4 -- 1000 elements
    ys <- shuffleM y20

    -- Sequential Data
    let xs = take 500 (drop 25000 [1..])
    let ys = xs ++ xs

    -- get no-cache
    startTime <- getPOSIXTime
    let ariaKeyList = map createAriaKey ys
    values <- mapM BPTree.get ariaKeyList
    let ariaValueList = map createAriaValue ys
    print $ values == ariaValueList
    endTime <- getPOSIXTime
    print (endTime - startTime)

    -- get cached
    lruCache <- newIORef (newLRU Cache.lruCacheSize)
    startTime <- getPOSIXTime
    let ariaKeyList = map createAriaKey ys
    values <- mapM (Cache.get lruCache) ariaKeyList
    let ariaValueList = map createAriaValue ys
    print $ values == ariaValueList
    endTime <- getPOSIXTime
    print (endTime - startTime)

