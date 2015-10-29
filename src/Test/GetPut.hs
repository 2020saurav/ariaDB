import AriaDB

data Person = Person {
    firstName :: String,
    lastName  :: String
} deriving (Show, Read, Eq)

foo = Person "John" "Doe"

main = do
    let testKey = "k102"
    put testKey foo
    bar <- get testKey
    print $ bar == foo
