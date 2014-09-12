module MSM where

-- we want to use monads here
import Control.Monad
import Data.Maybe
import Data.List as List

-- and you might also find Maps useful
import qualified Data.Map as Map

-- I want to debug stuff
import Debug.Trace as Trace

-- | The type of instructions for the MSM.
data Inst 
    = PUSH Int
    | POP
    | DUP
    | SWAP
    | NEWREG Int
    | LOAD
    | STORE
    | NEG
    | ADD
    | JMP
    | CJMP Int
    | HALT
    deriving (Eq,Show)
 
-- | The type `Prog` represents programs for the MSM.
type Prog = [Inst]

-- | The type `Stack` represents the stack of the MSM.
type Stack = [Int] 

-- | Regs is the type for keeping track of registers
type Regs = Map.Map Int Int


-- | This data type encapsulates the state of a running MSM.
data State = State
             { prog  :: Prog
             , pc    :: Int
             , stack :: Stack
             , regs  :: Regs
             }
           deriving (Show)

data ErrorType = StackUnderflow
               | UnallocatedRegister Int
               | RegisterAlreadyAllocated
               | InvalidPC
               | Unspec String
               deriving (Show, Read, Eq)
data Error = Error { errorType :: ErrorType, errorMessage ::  String}
           deriving (Show, Eq)


-- | `initial` constructs the initial state of an MSM running the
-- given program.
initial :: Prog -> State
initial p = State { prog = p
                  , pc = 0                     
                  , stack = []                   
                  , regs = Map.empty                      
                  }

-- | This is the monad that is used to implement the MSM. 
newtype MSM a = MSM (State ->Either ErrorType (a, State))

instance Monad MSM where
    -- return :: a -> MSM a
    return x = MSM (\s -> (x,s))
    
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    (MSM p) >>= f = MSM (\s -> let (aValue, previousState) = p s;
                                           (MSM newStatefulComputation) = f aValue
                                         in newStatefulComputation newState
                            )
    
    -- fail :: String -> MSM a
    fail s = MSM (\x -> (x,s)) -- ??

---- Remember to make your monad a functor as well
instance Functor MSM where
  --fmap :: (Functor f) => (a -> b) -> f a -> f b  
  f xs = xs >>= return . f

---- And perhaps also an Applicative
 instance Applicative MSM where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

-- | `get` returns the current state of the running MSM.
get :: MSM State
get = MSM $ \s -> (_,s)  

-- | set a new state for the running MSM.
set :: State -> MSM ()
set newState = MSM $ \s -> ((),newState)  

---- | modify the state for the running MSM according to
---- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = MSM $ \s ->  ((), f s)


---- | This function provides the instruction the PC currently points
---- to. If the PC is out of bounds, the MSM halts with an error.
--getInst :: ...
--getInst = ...


---- | This function runs the MSM.
--interp :: MSM ()
--interp = run
--    where run = do inst <- getInst
--                   cont <- interpInst inst
--                   when cont run

---- | This function interprets the given instruction. It returns True
---- if the MSM is supposed to continue it's execution after this
---- instruction.
--interpInst :: Inst -> MSM Bool
--interpInst inst = ...

---- | Run the given program on the MSM
--runMSM :: Prog -> ...
--runMSM p = let (MSM f) = interp 
--           in fmap snd $ f $ initial p



---- example program, when it terminates it leaves 42 on the top of the stack
--p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]

