module BPlusTree.Node where

import           Aria
import           BPlusTree.Types
import qualified Data.ByteString.Char8 as B

dataPath = "data/"

data Node = Node {
    keyCount :: Int,
    keys     :: [AriaKey],
    values   :: [BPTFileName],
    parent   :: Maybe BPTFileName
} deriving (Show, Read)

readNode :: BPTFileName -> IO Node
readNode nodeName = do
    fileContents <- B.readFile (dataPath ++ nodeName)
    let node = read (B.unpack fileContents) :: Node
    return node

writeNode :: BPTFileName -> Node -> IO ()
writeNode nodeName = B.writeFile (dataPath ++ nodeName) . B.pack . show
