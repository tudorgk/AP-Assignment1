-- Author: Tudor-Stefan Dragan


module Main where
import Test.HUnit
import Data.Either as Either
import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Test.HUnit.Tools (assertRaises)

import MSM

instance Eq ErrorCall where
    x == y = (show x) == (show y)

assertError msg ex f = 
    assertRaises msg (ErrorCall ex) $ evaluate f


-- check if [] 
emptyStack0 =  [POP, HALT]
emptyStack1 =  [DUP, HALT]
emptyStack2 =  [LOAD, HALT]
emptyStack3 =  [NEG, HALT]
emptyStack4 =  [JMP, HALT]
emptyStack5 =  [CJMP 1, HALT]

-- check if there are less than 2 elem
onlyOneElem0 =  [PUSH 1, ADD, HALT]
onlyOneElem1 =  [PUSH 1, STORE, HALT]
onlyOneElem2 =  [PUSH 1, SWAP, HALT]

-- check register not allocated
noReg =  [PUSH 1, LOAD, HALT]

-- check error on already allocated (NEWREG a)
allocSame =  [NEWREG 1, NEWREG 1, HALT]

-- PC out of bounds
outOfBoundsPC0 =  [PUSH 14, JMP, HALT]
outOfBoundsPC1 =  [PUSH (-4), JMP, HALT]

--Example program, when it terminates it leaves 42 on the top of the stack
p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]


testEmpty0 = TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM emptyStack0)

testEmpty1 =  TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM emptyStack1)

testEmpty2 =  TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM emptyStack2)

testEmpty3 =  TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM emptyStack4)

testEmpty4 =  TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM emptyStack4)

testEmpty5 =  TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM emptyStack5)

testonlyOneElem0 = TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM onlyOneElem0)

testonlyOneElem1 = TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM onlyOneElem1)

testonlyOneElem2 = TestCase $ assertError "Stack doesn't have enough elements" (show StackUnderflow) (MSM.runMSM onlyOneElem2)

testnoReg = TestCase $ assertError "Register not allocated" (show $ UnallocatedRegister 1) (MSM.runMSM noReg)

testallocSame = TestCase $ assertError "Register already allocated" (show RegisterAlreadyAllocated) (MSM.runMSM allocSame)

testoutOfBoundsPC0 = TestCase $ assertError "PC out of bounds" (show InvalidPC) (MSM.runMSM outOfBoundsPC0)

testoutOfBoundsPC1 = TestCase $  assertError "PC out of bounds" (show InvalidPC)  (MSM.runMSM outOfBoundsPC1)

testprogram42 = TestCase $ assertBool "Program 42 works" $ 42 == (MSM.runMSM p42)

tests = TestList [TestLabel "MSM testsuite" $ TestList [testEmpty0,testEmpty1,testEmpty2,testEmpty3,testEmpty4,testEmpty5,testonlyOneElem0,testonlyOneElem1,testonlyOneElem2,testnoReg,testallocSame,testoutOfBoundsPC0,testoutOfBoundsPC1,testprogram42]]

-- main
main = do
	runTestTT tests