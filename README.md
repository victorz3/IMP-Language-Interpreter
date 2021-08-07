# IMP LANGUAGE INTERPRETER

This is an interpreter for the IMP language. The IMP language is the one
specified by the following grammar:

P → skip | X := A | (P;P) | (if B then P else P) | (while B do P)<br/>
A → N | (A + A) | (A - A) | (A × A)<br/>
B → true | false | (A = A) | (A < A) | ¬B | (B ∨ B) | (B ∧ B)<br/>
X → x[N]<br/>
N → 0 | C<br/>
C → 1S | 2S | 3S | 4S | 5S | 6S | 7S | 8S | 9S<br/>
S → 0S | 1S | 2S | 3S | 4S | 5S | 6S | 7S | 8S | 9S | ϵ<br/>

The interpreter includes the following features:

- Execution of a list of programs (either sequential or parallel).<br/>
- Optional maximum number of steps to execute (default value 10000). This must
  be specified as a number in the first line of a program file.<br/>
- Checking of results' correctness.<br/>

------------------------------PREREQUISITES------------------------------------<br/>
Install The Haskell Tool Stack. Instructions can be found on the official
website:<br/>
https://docs.haskellstack.org/en/stable/README/#how-to-install


------------------------------INSTRUCTIONS-------------------------------------<br/>
To run a list of programs the `Data` folder must contain:<br/>
1) The list of programs to run, one per line, in a file named `programs.txt`.<br/>
2) The programs to run in the `programs` folder. The programs must be correct
IMP programs with `.imp` extension.<br/>

Then simply change into the `Interpreter` folder and run the following commands:

```console
$ stack build
Some output.
$ stack exec Intepreter-exe
```

If you want to run the interpreter in parallel mode, instead of `stack exec
Interpreter-exe`, use `stack exec -- Interpreter-exe -par`. Do note that
parallel mode may use a lot of memory (about 5 GB to run 500000 programs in
parallel).

The outputs of your programs will be in a file called `outputs.txt` in the
`Data` folder. A file `hash.txt` will also be created in the same folder. This
file contains the SHA256 sum of the outputs file.
