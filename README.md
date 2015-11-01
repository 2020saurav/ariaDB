# ariaDB - Datastore for Haskell Types

### Build:
To build and run AriaDB service (from project root):
```shell
  $ cabal run
```

### Test:
```shell
	$ touch testdb
	$ echo "[]" > testdb
	$ cabal run &
	$ cd src/Test
	$ runhaskell -i../Library GetPut.hs
```
