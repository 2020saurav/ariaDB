module GetPutDelete where

import AriaDB

data Person = Person {
    firstName :: String,
    lastName  :: String
} deriving (Show, Read, Eq)

foo = Person "John" "Doe"

main = do
    let testKey = "k1"
    put testKey foo

    bar <- get testKey
    case bar of
        Just v  -> print (v::Person) -- Type Assigned
        Nothing -> print "Nothing"

    baz <- get testKey
    case baz of
        Just v  -> print $ v == foo -- Type Inferred
        Nothing -> print "Value not found"

    delete testKey
    qux <- get testKey
    case qux of
        Just v  -> print (v::Person) -- Type Assigned
        Nothing -> print "Nothing"
