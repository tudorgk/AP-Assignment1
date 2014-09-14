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

I've tested all of the base cases I could think of. Unfortunately I didn't have time to test other "out of this world" tests.


