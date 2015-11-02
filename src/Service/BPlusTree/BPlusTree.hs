module BPlusTree.BPlusTree where

import           Aria
import           BPlusTree.Helper as H
import qualified BPlusTree.Leaf as L
import qualified BPlusTree.Node as N
import           BPlusTree.Types
import           Control.Monad
import           Data.List as DL

m = 6 -- experiment with this and fix this value in this file

upsertInLeaf :: BPTFileName -> AriaKV -> IO ()
upsertInLeaf leafName kv = do
    leaf <- L.readLeaf leafName
    let index = H.findPosition (L.keys leaf) (ariaKey kv)
    if (L.keyCount leaf > index) && (L.keys leaf !! index) == ariaKey kv then do -- update
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

findLeaf :: AriaKey -> IO BPTFileName
findLeaf key = do
    rootName <- H.getRootName
    findLeafAux key rootName

findLeafAux :: AriaKey -> BPTFileName -> IO BPTFileName
findLeafAux key current = if H.isLeaf current then return current
    else do
        node <- N.readNode current
        let index = H.findPosition (N.keys node) key
        if (length (N.keys node) > index) && (N.keys node !! index) == key then
            findLeafAux key (N.values node !! (index+1))
        else findLeafAux key (N.values node !! index)

upsert :: AriaKV -> IO ()
upsert kv = do
    leafName <- findLeaf $ ariaKey kv
    upsertInLeaf leafName kv

get :: AriaKey -> IO (Maybe AriaValue)
get key = do
    leafName <- findLeaf key
    leaf <- L.readLeaf leafName
    let index = H.findPosition (L.keys leaf) key
    if index >= L.keyCount leaf then return Nothing
    else if (L.keys leaf !! index) == key then
        return (L.values leaf !! index)
        else return Nothing
