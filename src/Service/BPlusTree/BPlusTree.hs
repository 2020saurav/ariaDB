{-|
Module      : BPlusTree
Description : B+ Tree to index the data store
License     : BSD3
Maintainer  : 2020saurav@gmail.com
Stability   : experimental

B+ is an n-ary tree with variable (m) number of children per node.
A B+ Tree consists of a root, internal nodes and leaves.
The root may either be a leaf or a node with 2 or more children.
This is a disk based implementation of B+ Tree.
Wikipedia page on <https://en.wikipedia.org/wiki/B%2B_tree>
-}

module BPlusTree.BPlusTree (upsert, get, remove) where

import           Aria
import           BPlusTree.Helper as H
import qualified BPlusTree.Leaf as L
import qualified BPlusTree.Node as N
import           BPlusTree.Types
import           Control.Monad
import           Data.List as DL

-- | 'm' is the maximum number of children a node can have. In order to
-- maintain the B+ Tree property, a node (and a leaf) is split into 2 when
-- this maximum number is reached. In a strictly efficient implementation,
-- 'm' is set such that the number of bytes in a file a multiple of hard-disk
-- page size, otherwise the file is appended with garbage to make it so.
-- For experimental purposes, I am using m = 16, as it empirically gave better
-- read/write time (for 5000 keys)
m = 16

-- | 'upsertInLeaf' is an auxilliary function to upsert a key-value pair in a
-- given file. The function finds the right position to insert the key, and if
-- the key is already present, the corresponding value is updated and keyCount
-- remains the same. Whereas, if the key is not present, key value pair is
-- inserted at the right place, increasing the keyCount by 1
-- Finally, if the keyCount == m (the maximum limit of no. of children), the
-- leaf is split.
upsertInLeaf :: BPTFileName -> AriaKV -> IO ()
upsertInLeaf leafName kv = do
    leaf <- L.readLeaf leafName
    let index = H.findPosition (L.keys leaf) (ariaKey kv)
    if (L.keyCount leaf > index) && (L.keys leaf !! index) == ariaKey kv then do
        -- update
        let newLeaf = L.Leaf {
            L.keyCount = L.keyCount leaf,
            L.keys     = H.updateAt index (ariaKey kv) (L.keys leaf),
            L.values   = H.updateAt index (Just $ ariaValue kv) (L.values leaf),
            L.parent   = L.parent leaf,
            L.left     = L.left leaf,
            L.right    = L.right leaf
        }
        L.writeLeaf leafName newLeaf
    else do -- insert
        let newLeaf = L.Leaf {
            L.keyCount = L.keyCount leaf + 1,
            L.keys     = H.insertAt index (ariaKey kv) (L.keys leaf),
            L.values   = H.insertAt index (Just $ ariaValue kv) (L.values leaf),
            L.parent   = L.parent leaf,
            L.left     = L.left leaf,
            L.right    = L.right leaf
        }
        L.writeLeaf leafName newLeaf
        when (L.keyCount newLeaf == m) $ splitLeaf leafName

-- | 'insertInNode' is an auxilliary function to insert a key and corresponding
-- file pointer in a given node file. The function finds the right position to
-- insert the key and performs the insertion. If the keyCount reaches the max
-- limit, it is split.
insertInNode :: BPTFileName -> AriaKey -> BPTFileName -> IO ()
insertInNode nodeName key subTree = do
    node <- N.readNode nodeName
    let index = H.findPosition (N.keys node) key
    let newNode = N.Node {
        N.keyCount = N.keyCount node + 1,
        N.keys     = H.insertAt index key (N.keys node),
        N.values   = H.insertAt (index + 1) subTree (N.values node),
        N.parent   = N.parent node
    }
    N.writeNode nodeName newNode
    when (N.keyCount newNode == m) $ splitNode nodeName

-- | 'splitLeaf' is to split a leaf into two leaves and correctly adjust parent
-- and sibling pointers. First half key values are kept in the original leaf,
-- and remaining are shifted to the new sibling. Sibling pointers on left and
-- right are appropriately adjusted, and the middle key is propagated upward to
-- the parent for insertion. Case for root leaf is specially handled.
splitLeaf :: BPTFileName -> IO ()
splitLeaf leafName = do
    leaf <- L.readLeaf leafName
    let (k1, k2) = splitAt (m `quot` 2) (L.keys   leaf)
    let (v1, v2) = splitAt (m `quot` 2) (L.values leaf)
    newLeafName <- H.getNewLeafName
    let leaf1 = L.Leaf {
        L.keyCount = m `quot` 2,
        L.keys     = k1,
        L.values   = v1,
        L.parent   = L.parent leaf,
        L.left     = L.left leaf,
        L.right    = Just newLeafName
    }
    let leaf2 = L.Leaf {
        L.keyCount = m - (m `quot` 2),
        L.keys     = k2,
        L.values   = v2,
        L.parent   = L.parent leaf,
        L.left     = Just leafName,
        L.right    = L.right leaf
    }
    L.writeLeaf leafName    leaf1
    L.writeLeaf newLeafName leaf2
    case L.right leaf of
        Nothing -> return ()
        Just r  -> H.updateLeftPtr r newLeafName
    let midKey = L.keys leaf !! (m `quot` 2)
    case L.parent leaf of
        Nothing -> do               -- no parent => this is root
            rootName <- H.createNewRoot midKey leafName newLeafName
            H.updateParent rootName leafName
            H.updateParent rootName newLeafName
        Just parent -> insertInNode parent midKey newLeafName

-- | 'splitNode' is to split a node into two nodes and correctly adjust parent
-- and children's parent pointers. Half of the children will now have a new
-- parent, so they must know about it. First half key values are kept in the
-- original node, and remaining are shifted to the new node. The middle key is
-- propagated upwards. Case for root node is specially handled.
splitNode :: BPTFileName -> IO ()
splitNode nodeName = do
    node <- N.readNode nodeName
    let k1 = take (m `quot` 2)     (N.keys   node)
    let k2 = drop (m `quot` 2 + 1) (N.keys   node)
    let v1 = take (m `quot` 2 + 1) (N.values node)
    let v2 = drop (m `quot` 2 + 1) (N.values node)
    newNodeName <- H.getNewNodeName
    let node1 = N.Node {
        N.keyCount = m `quot` 2,
        N.keys     = k1,
        N.values   = v1,
        N.parent   = N.parent node
    }
    let node2 = N.Node {
        N.keyCount = m - (m `quot` 2) - 1,
        N.keys     = k2,
        N.values   = v2,
        N.parent   = N.parent node
    }
    N.writeNode nodeName    node1
    N.writeNode newNodeName node2
    mapM_ (H.updateParent newNodeName) (N.values node2)
    let midKey = N.keys node !! (m `quot` 2)
    case N.parent node of
        Nothing -> do
            rootName <- H.createNewRoot midKey nodeName newNodeName
            H.updateParent rootName nodeName
            H.updateParent rootName newNodeName
        Just parent -> insertInNode parent midKey newNodeName

-- | 'findLeaf' finds the name of the leaf file which contains (if at all) the
-- given key.
findLeaf :: AriaKey -> IO BPTFileName
findLeaf key = do
    rootName <- H.getRootName
    findLeafAux key rootName

-- | 'findLeafAux' is auxilliary to the above function. It traverses the tree to
-- determine the name of the leaf which contains (if at all) the given key.
findLeafAux :: AriaKey -> BPTFileName -> IO BPTFileName
findLeafAux key current = if H.isLeaf current then return current
    else do
        node <- N.readNode current
        let index = H.findPosition (N.keys node) key
        if (length (N.keys node) > index) && (N.keys node !! index) == key then
            findLeafAux key (N.values node !! (index+1))
        else findLeafAux key (N.values node !! index)

-- | 'upsert' method takes in a 'AriaKV' key value, and if the key is already
-- present, it updates the corresponding 'AriaValue', else it inserts the
-- key value pair in the right position in the B+ Tree.
upsert :: AriaKV -> IO ()
upsert kv = do
    leafName <- findLeaf $ ariaKey kv
    upsertInLeaf leafName kv

-- | 'get' method takes in a 'AriaKey' key and returns the value inside 'Maybe'
-- monad, i.e 'Just' value or 'Nothing'
get :: AriaKey -> IO (Maybe AriaValue)
get key = do
    leafName <- findLeaf key
    leaf <- L.readLeaf leafName
    let index = H.findPosition (L.keys leaf) key
    if index >= L.keyCount leaf then return Nothing
    else if (L.keys leaf !! index) == key then
        return (L.values leaf !! index)
        else return Nothing

-- | 'remove' method takes in a 'AriaKey' key and sets the corresponding
-- 'AriaValue' to 'Nothing'. Since deletion is a B+ Tree is a heavy and complex
-- operation, in practice, after certain period of time the data is collected
-- and re-indexed to remove "deleted" key values.
remove :: AriaKey -> IO ()
remove key = do
    leafName <- findLeaf key
    leaf <- L.readLeaf leafName
    let index = H.findPosition (L.keys leaf) key
    if (L.keyCount leaf > index) && (L.keys leaf !! index) == key then do
        let newLeaf = L.Leaf {
            L.keyCount = L.keyCount leaf,
            L.keys     = L.keys leaf,
            L.values   = H.updateAt index Nothing (L.values leaf),
            L.parent   = L.parent leaf,
            L.left     = L.left leaf,
            L.right    = L.right leaf
        }
        L.writeLeaf leafName newLeaf
    else return () -- key doesn't exist
