-- This code is used to test file IO and its interaction with the Warp API.
-- Actual code will have B+ Tree implementation. This code is very simplistic,
-- but covers all important aspects (walls) that we will hit in the project.
-- BPlusTree module will export same functions as this and no changes should be
-- made anywhere else. Just indexing tree will shift from UTree to BPTree

module UnaryTree where -- TODO write exports

import           Aria
import qualified Data.ByteString.Char8 as B

type UTree       = [AriaKV]
type TempAriaKey = Char

dbFileName :: FilePath
dbFileName = "testdb"
-- The file must not be nil. Atleast put "[]"

db :: IO UTree
db = do
    contents <- B.readFile dbFileName
    return (read (B.unpack contents) :: UTree)

-- This implementation will just ignore the key for now
insert :: AriaKey -> AriaValue -> IO ()
insert key value = do
    utree <- db
    let newutree = utree ++ [AriaKV key value]
    B.writeFile dbFileName . B.pack . show $ newutree
    return ()
