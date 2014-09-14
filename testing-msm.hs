module Main where
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import MSM

data Expr = Con Int
          | Add Expr Expr
          deriving (Eq, Show)

value :: Expr -> Int
value (Con n) = n
value (Add x y) = value x + value y

compile :: Expr -> Prog
compile e = comp e [HALT]
  where comp (Con n) prog = PUSH n : prog
        comp (Add e1 e2) prog = comp e2 $ comp e1 $ ADD : prog

main = quickCheck $ runMSM [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]