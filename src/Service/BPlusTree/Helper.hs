module BPlusTree.Helper where

import           Aria
import           BPlusTree.Types
import qualified Data.ByteString.Char8 as B
import qualified BPlusTree.Leaf as L
import qualified BPlusTree.Node as N

mdf = "BPlusTree/metadata"

data MetaData = MetaData {
    leafCount :: Int,
    nodeCount :: Int,
    root      :: BPTFileName
} deriving (Show, Read)

isLeaf :: BPTFileName -> Bool
isLeaf (x:xs) = if x=='L' then True else False

getNewLeafName :: IO String
getNewLeafName = do
    content <- B.readFile mdf
    let metadata = read (B.unpack content) :: MetaData
    let newLeafName = "L" ++ (show $ 1 + leafCount metadata)
    let newMetaData = MetaData (1 + leafCount metadata) (nodeCount metadata) (root metadata)
    B.writeFile mdf . B.pack . show $ newMetaData
    return newLeafName

getNewNodeName :: IO String
getNewNodeName = do
    content <- B.readFile mdf
    let metadata = read (B.unpack content) :: MetaData
    let newNodeName = "N" ++ (show $ 1 + nodeCount metadata)
    let newMetaData = MetaData (leafCount metadata) (1 + nodeCount metadata) (root metadata)
    B.writeFile mdf . B.pack . show $ newMetaData
    return newNodeName

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

updateParent :: BPTFileName -> BPTFileName -> IO ()
updateParent parentName fileName = do
    if isLeaf fileName then do
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

updateRootName :: BPTFileName -> IO ()
updateRootName newRoot = do
    content <- B.readFile mdf
    let metadata = read (B.unpack content) :: MetaData
    let newMetaData = MetaData (leafCount metadata) (nodeCount metadata) (newRoot)
    B.writeFile mdf . B.pack . show $ newMetaData
