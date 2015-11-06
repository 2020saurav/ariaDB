{-|
Module      : BPlusTree.Node
Description : Node module for B+ Tree
License     : BSD3
Maintainer  : 2020saurav@gmail.com
Stability   : experimental

This module provides data type 'Node' which corresponds to a node in a B+ tree.
A node contains information about the number of keys, list of keys, list of
values and file name (file pointer) to the parent node. The values corresponding
to the keys are file names (pointers) to the children nodes.
-}
module BPlusTree.Node where

import           Aria
import           BPlusTree.Types
import qualified Data.ByteString.Char8 as B

-- | Pathname of the directory storing the database, relative to project root
dataPath :: FilePath
dataPath = "data/"

data Node = Node {
    keyCount :: Int,                -- ^ Number of keys in the node
    keys     :: [AriaKey],          -- ^ List of keys (sorted)
    values   :: [BPTFileName],      -- ^ List of values (pointers to children)
    parent   :: Maybe BPTFileName   -- ^ FileName of the parent node
} deriving (Show, Read)

-- | It reads the content of the file with given file name and returns the
-- 'Node' object inside IO monad.
readNode :: BPTFileName -> IO Node
readNode nodeName = do
    fileContents <- B.readFile (dataPath ++ nodeName)
    let node = read (B.unpack fileContents) :: Node
    return node

-- | It writes the 'show' value of given 'Leaf' object to the file with given
-- name.
writeNode :: BPTFileName -> Node -> IO ()
writeNode nodeName = B.writeFile (dataPath ++ nodeName) . B.pack . show
