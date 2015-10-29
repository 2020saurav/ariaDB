# ariaDB - Datastore for Haskell Types

### Build:
To build and run AriaDB service (from project root):
```shell
cabal run
```

### Test:
- Create a temporary file, say "testdb", in project root (don't forget to add that to .gitignore)
- For the first time, when testdb is empty, put "[]" (without quotes) in it [temporary hack]
- Run the AriaDB service
- cd src/Test
```shell
runhaskell -i ../Library GetPut.hs
```
