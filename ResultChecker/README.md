# ResultChecker

This module is for checking correctness of results. The module requires that the
`hash.txt`, `outputs.txt`, and `programs.txt` files be in the `../Data` folder.

To check the results, simply run:

```console
$ stack build
some output
$ stack exec ResultChecker-exe
```
