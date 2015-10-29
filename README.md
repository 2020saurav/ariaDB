# ariaDB

Datastore for Haskell Types

### Build, Run, Test
To run AriaDB service (from project root):
```shell
cabal run
```
To test:
- Create a temp file "testdb" in project root (do not commit)
- For the first time, when testdb is empty, put "[]" (without quotes) in it [temporary hack]
- Run the AriaDB service
- cd src/Test
-   ```shell
        runhaskell -i../Library  GetPut.hs
    ```

### TODO
- Replace UnaryTree (basically, a list with FileIO) with BPlusTree
- Implement deletion
- Make storage efficient in terms of redundant information being saved on disk
- ...
