# ariaDB [![Build Status](https://travis-ci.org/proneetv/ariaDB.svg?branch=master)](https://travis-ci.org/proneetv/ariaDB)
ariaDB is a persistent key-value store written in Haskell.

### Features
- Key-Value store as a service exposing REST API
- Client library (for Haskell) to interact with the API
- Simple interface : `get`, `put` and `delete`
- B+ Tree indexing for fast access
- Type Safe (entire code in Haskell)
- Stores object of any type
- Concurrent access to the store (Warp)

### Usage (Haskell)
```haskell
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
```

### Build
To build and run AriaDB service (from project root):
```shell
  $ cabal run
```

### Test
```shell
	$ cabal run &
	$ cd src/Test
	$ runhaskell -i../Library GetPutDelete.hs
```
