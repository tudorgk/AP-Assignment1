[AP-Assignment1](https://github.com/tudorgk/AP-Assignment0)
==============

The virtual machine, MicroStackMachine (MSM), consists of:  a program counter (PC),  a dynamically allocated number of integer registers, and  a stack of integers.  A program for the MSM is a zero-indexed sequence of instructions. The execution of a program in the MSM might halt with an error.


SYNOPSIS
--------

Example of usage:

#####Testing in HUNIT

```bash
user:shell$ ghc testing-msm.hs 
user:shell$ ./testing-msm
```

#####Running in ghci

```haskell
ghci> runMSM [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]
```

DESCRIPTION 
-----------

For testing I've used HUnit. I've wrote some basic unit test that check for the output of certain program instructions. Please install `testpack` as well in order to assert the error.

I've figured out how to extract the last element from the stack :). The `runMSM` function takes care of that. If the left side is returned I display an error

I've modified the stack manipulation functions so that they use each other (for ex. `dup` function uses `push` and `pop` respectively)



