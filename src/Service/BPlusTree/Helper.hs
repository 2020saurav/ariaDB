{-|
Module      : BPlusTree.Helper
Description : Helper functions for handling operations in B+ Tree
License     : BSD3
Maintainer  : 2020saurav@gmail.com
Stability   : experimental

B+ is an n-ary tree with variable (m) number of children per node.
A B+ Tree consists of a root, internal nodes and leaves.
The root may either be a leaf or a node with 2 or more children.
This is a disk based implementation of B+ Tree.
Wikipedia page on <https://en.wikipedia.org/wiki/B%2B_tree>
This module contains helper functions for operations on B+ Tree.
-}
module BPlusTree.Helper where

import           Aria
import           BPlusTree.Types
import qualified Data.ByteString.Char8 as B
import qualified BPlusTree.Leaf as L
import qualified BPlusTree.Node as N

-- | 'mdf' is the relative 'FilePath' of metadata file which contains
-- information about number of leaves, nodes and current root in the B+ Tree.
mdf :: FilePath
mdf = "data/metadata"

data MetaData = MetaData {
    leafCount :: Int,        -- ^ Number of leaves in the B+ Tree
    nodeCount :: Int,        -- ^ Number of nodes in the B+ Tree
    root      :: BPTFileName -- ^ Current root of the B+ Tree
} deriving (Show, Read)

-- | A boolean function to test if a file name corresponds to a leaf.
isLeaf :: BPTFileName -> Bool
isLeaf (x:xs) = x=='L'

-- | Finds the position of a 'AriaKey' in a sorted list of 'AriaKey' such that
-- the list is sorted after insertion of the key at that position. In other
-- words, it returns the first position where the key is greater (or equal) to
-- this key.
findPosition :: [AriaKey] -> AriaKey -> Int
findPosition keys key = findPositionAux keys key 0

-- | Auxilliary function for above 'findPosition' function to find position for
-- a 'AriaKey' in a list of 'AriaKey'.
findPositionAux :: [AriaKey] -> AriaKey -> Int -> Int
findPositionAux keys key index = case keys of
    x:xs -> if x >= key then index
            else findPositionAux xs key index + 1
    _    -> index

-- | It takes in an index, a value and list and inserts the value in the list at
-- the given index.
insertAt :: Int -> a -> [a] -> [a]
insertAt index x xs = take index xs ++ [x] ++ drop index xs

-- | It takes in an index, a value and list and updates the value in the list at
-- the given index.
updateAt :: Int -> a -> [a] -> [a]
updateAt index x xs = take index xs ++ [x] ++ drop (index+1) xs

-- | Generates the next name for leaf, adding 1 to the current.
getNewLeafName :: IO String
getNewLeafName = do
    content <- B.readFile mdf
    let metadata = read (B.unpack content) :: MetaData
    let newLeafName = "L" ++ show (1 + leafCount metadata)
    let newMetaData = MetaData (1 + leafCount metadata) (nodeCount metadata)
            (root metadata)
    B.writeFile mdf . B.pack . show $ newMetaData
    return newLeafName

-- | Generates the next name for node, adding 1 to the current.
getNewNodeName :: IO String
getNewNodeName = do
    content <- B.readFile mdf
    let metadata = read (B.unpack content) :: MetaData
    let newNodeName = "N" ++ show (1 + nodeCount metadata)
    let newMetaData = MetaData (leafCount metadata) (1 + nodeCount metadata)
            (root metadata)
    B.writeFile mdf . B.pack . show $ newMetaData
    return newNodeName

-- | It takes in a leaf file name and updates it's left value to the given value
updateLeftPtr :: BPTFileName -> BPTFileName -> IO ()
updateLeftPtr leafName value = do
    leaf <- L.readLeaf leafName
    let newLeaf = L.Leaf {
        L.keyCount = L.keyCount leaf,
        L.keys     = L.keys leaf,
        L.values   = L.values leaf,
        L.parent   = L.parent leaf,
        L.left     = Just value,
        L.right    = L.right leaf
    }
    L.writeLeaf leafName newLeaf

-- | It takes in a 'AriaKey' and two child pointer names and creates a new root.
createNewRoot :: AriaKey -> BPTFileName -> BPTFileName -> IO String
createNewRoot key child1 child2 = do
    nodeName <- getNewNodeName
    let newNode = N.Node {
        N.keyCount = 1,
        N.keys     = [key],
        N.values   = [child1, child2],
        N.parent   = Nothing
    }
    N.writeNode nodeName newNode
    updateRootName nodeName
    return nodeName

-- | It takes in name of the parent node and a child node (or leaf), and sets
-- child node's (or leaf's) parent to be given value.
updateParent :: BPTFileName -> BPTFileName -> IO ()
updateParent parentName fileName = if isLeaf fileName then do
    leaf <- L.readLeaf fileName
    let newLeaf = L.Leaf {
        L.keyCount = L.keyCount leaf,
        L.keys     = L.keys leaf,
        L.values   = L.values leaf,
        L.parent   = Just parentName,
        L.left     = L.left leaf,
        L.right    = L.right leaf
    }
    L.writeLeaf fileName newLeaf
else do
    node <- N.readNode fileName
    let newNode = N.Node {
        N.keyCount = N.keyCount node,
        N.keys     = N.keys node,
        N.values   = N.values node,
        N.parent   = Just parentName
    }
    N.writeNode fileName newNode

-- | It updates the metadata with new root node name.
updateRootName :: BPTFileName -> IO ()
updateRootName newRoot = do
    content <- B.readFile mdf
    let metadata = read (B.unpack content) :: MetaData
    let newMetaData = MetaData (leafCount metadata) (nodeCount metadata) newRoot
    B.writeFile mdf . B.pack . show $ newMetaData

-- | It gives the name of the node (or leaf) of the current root.
getRootName :: IO BPTFileName
getRootName = do
    content <- B.readFile mdf
    let metadata = read (B.unpack content) :: MetaData
    return (root metadata)
