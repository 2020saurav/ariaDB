{-|
Module      : BPlusTree.Leaf
Description : Leaf module for B+ Tree
License     : BSD3
Maintainer  : 2020saurav@gmail.com
Stability   : experimental

This module provides data type 'Leaf' which corresponds to a leaf in a B+ tree.
A leaf contains information about the number of keys, list of keys, list of
values, file name (file pointer) to the parent node, file pointer to the left
and right siblings.
-}
module BPlusTree.Leaf where

import           Aria
import           BPlusTree.Types
import qualified Data.ByteString.Char8 as B

-- | Pathname of the directory storing the database, relative to project root
dataPath :: FilePath
dataPath="data/"

data Leaf = Leaf {
    keyCount :: Int,                -- ^ Number of keys in the leaf
    keys     :: [AriaKey],          -- ^ List of keys (sorted)
    values   :: [Maybe AriaValue],  -- ^ List of corresponding values. When a
                                    -- value is deleted, it is set to 'Nothing'
    parent   :: Maybe BPTFileName,  -- ^ FileName of the Parent node. 'Nothing'
                                    -- when it is the root itself
    left     :: Maybe BPTFileName,  -- FileName of the left sibling
    right    :: Maybe BPTFileName   -- FileName of the right sibling
} deriving (Show, Read)

-- | It reads the content of the file with given file name and returns the
-- 'Leaf' object inside IO monad.
readLeaf :: BPTFileName -> IO Leaf
readLeaf leafName = do
    fileContents <- B.readFile (dataPath ++ leafName)
    let leaf = read (B.unpack fileContents) :: Leaf
    return leaf


-- | It writes the 'show' value of given 'Leaf' object to the file with given
-- name.
writeLeaf :: BPTFileName -> Leaf -> IO ()
writeLeaf leafName = B.writeFile (dataPath ++ leafName) . B.pack . show
