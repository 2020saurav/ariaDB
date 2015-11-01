module BPlusTree.Leaf where

import           Aria
import           BPlusTree.Types
import qualified Data.ByteString.Char8 as B

data Leaf = Leaf {
    keyCount :: Int,
    keys     :: [AriaKey],
    values   :: [Maybe AriaValue], -- deleted values will become Nothing
    parent   :: Maybe BPTFileName,
    left     :: Maybe BPTFileName,
    right    :: Maybe BPTFileName
} deriving (Show, Read)

readLeaf :: BPTFileName -> IO Leaf
readLeaf leafName = do
    fileContents <- B.readFile leafName
    let leaf = read (B.unpack fileContents) :: Leaf
    return leaf

writeLeaf :: BPTFileName -> Leaf -> IO ()
writeLeaf leafName = B.writeFile leafName . B.pack . show
