module BPlusTree.BPlusTree where

import           Aria
import           BPlusTree.Helper as H
import qualified BPlusTree.Leaf as L
import qualified BPlusTree.Node as N
import           BPlusTree.Types
import           Data.List as DL

m = 6 -- experiment with this and fix this value in this file

upsertInLeaf :: BPTFileName -> AriaKV -> IO ()
upsertInLeaf leafName kv = do
    leaf <- L.readLeaf leafName
    -- TODO find correct position and upsert the value
    let newLeaf = L.Leaf {
        L.keyCount = L.keyCount leaf + 1,
        L.keys     = L.keys leaf ++ [ariaKey kv],
        L.values   = L.values leaf ++ [Just $ ariaValue kv],
        L.parent   = L.parent leaf,
        L.left     = L.left leaf,
        L.right    = L.right leaf
    }
    L.writeLeaf leafName newLeaf
    if L.keyCount newLeaf == m then
        splitLeaf leafName
        else return ()

insertInNode :: BPTFileName -> AriaKey -> BPTFileName -> IO ()
insertInNode nodeName key subTree = do
    node <- N.readNode nodeName
    -- TODO add key and subTree in correct position
    let newNode = N.Node {
        N.keyCount = N.keyCount node + 1,
        N.keys     = N.keys node ++ [key],
        N.values   = N.values node ++ [subTree],
        N.parent   = N.parent node
    }
    N.writeNode nodeName newNode
    if N.keyCount newNode == m then
        splitNode nodeName
        else return ()

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

    let midKey = (L.keys leaf) !! (m `quot` 2)
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
    -- TODO : Worried about laziness here! This monadic value may not act at all (?)
    return $ map (H.updateParent newNodeName) (N.values node2)
    let midKey = (N.keys node) !! (m `quot` 2)
    case N.parent node of
        Nothing -> do
            rootName <- H.createNewRoot midKey nodeName newNodeName
            H.updateParent rootName nodeName
            H.updateParent rootName newNodeName
        Just parent -> insertInNode parent midKey newNodeName
