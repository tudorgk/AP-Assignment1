-- Author: Tudor-Stefan Dragan


module MSM (runMSM) where

-- we want to use monads here
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List as List


-- and you might also find Maps useful
import qualified Data.Map as Map

-- trying to debug stuff
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
data Error = Error { errorType :: ErrorType}
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
newtype MSM a = MSM { unMSM :: State -> Either Error (a, State) }

instance Monad MSM where
    -- return :: a -> MSM a
    return a = MSM (\x -> Right (a,x))
    
    -- fail :: String -> MSM a
    fail s = MSM (\x -> Left s)
    
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    -- trying to write it like this so I can UNDERSTAND!!
    (MSM ma) >>= func = MSM $ \ st -> do (a, st') <- ma st
                                         unMSM (func a) st'

---- Remember to make your monad a functor as well
instance Functor MSM where
  --fmap :: (Functor f) => (a -> b) -> f a -> f b  
  fmap f xs = xs >>= return . f

---- And perhaps also an Applicative
instance Applicative MSM where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

-- helper function to trace the monad
traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = trace ("test: " ++ show x) (return x)

-- | `get` returns the current state of the running MSM.
get :: MSM State
get = MSM (\x -> Right (x,x))

-- | set a new state for the running MSM.
set :: State -> MSM ()
--set m| Trace.trace("called set state " ++ show m) False = undefined
set m = MSM (\x -> Right ((),m))

-- | modify the state for the running MSM according to
-- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = MSM (\s -> Right ((), f s))


-- | This function provides the instruction the PC currently points
-- to. If the PC is out of bounds, the MSM halts with an error.
getInst :: MSM Inst
getInst = do
  state <- get -- get the state
  if pc state > length (prog state) || pc state < 0 -- check pc from state bounds
    then fail (decodeError Error {errorType = InvalidPC}) -- respond with an error
    else return $ prog state !! pc state-- return the inst at the PC index from the Prog list

-- | This function runs the MSM.
interp :: MSM ()
interp = run
    where run = do inst <- getInst
                   cont <- interpInst inst
                   when cont run -- cont needs to be Bool so how do I return an Int from pop??

-- | This function interprets the given instruction. It returns True
-- if the MSM is supposed to continue it's execution after this
-- instruction.
interpInst :: Inst -> MSM Bool
interpInst inst = do
  currentState <- get
  set currentState {pc = pc currentState + 1}
  --traceMonad currentState -- show the state for debugging
  case inst of 
    PUSH a -> do 
      push a 
      return True
    POP -> do 
      x <- pop 
      return True
    DUP -> do
      dup 
      return True
    SWAP -> do
      swap 
      return True
    NEWREG a -> do
      newreg a 
      return True
    LOAD -> do
      load 
      return True
    STORE -> do
      store 
      return True
    NEG -> do
      neg 
      return True
    ADD -> do
      add 
      return True
    JMP -> do
      jmp 
      return True
    CJMP a -> do
      cjmp a 
      return True
    HALT -> do -- Stop! Cease and desist!
      return False
    


push :: Int -> MSM ()  
push a  = do
  aState <- get
  set aState{stack = a:stack aState}

pop ::  MSM Int -- how do I return Int if cont expects a Bool value??
pop  = do 
  aState <- get
  let (first:others) = stack aState
  let checker | List.null (stack aState) = fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = set aState{stack = others}  
  checker
  return (head (stack aState)) 

dup :: MSM ()
dup = do
  x <- pop
  push x
  push x
   

swap :: MSM ()
swap = do
  x <- pop
  y <- pop
  push x 
  push y
  

newreg :: Int -> MSM ()
newreg aReg = do
  aState <- get
  let checker | Map.member aReg (regs aState) = 
                  fail $ decodeError Error {
                    errorType = RegisterAlreadyAllocated
                  }
              | otherwise =
                  set aState { 
                    regs = Map.insert aReg 0 (regs aState)
                  }
  checker
  

load :: MSM ()
load = do
  aState <- get
  let checker | List.null (stack aState) = 
                  fail $ decodeError Error{errorType = StackUnderflow}
              | not (Map.member (head (stack aState)) (regs aState)) = 
                  fail $ decodeError Error {
                    errorType = UnallocatedRegister (head (stack aState)) 
                  }
              | otherwise = 
                  set aState{
                    stack = regs aState Map.! head (stack aState) : tail (stack aState)
                  }
  checker


store ::  MSM ()
store = do
  aState <- get
  let checker | not (Map.member (stack aState !! 1) (regs aState)) =
                  fail $ decodeError Error {
                    errorType = UnallocatedRegister (stack aState !! 1)
                  }   
              | length (stack aState) < 2 = 
                  fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = 
                  set aState {
                    regs = Map.insert (stack aState !! 1) (head (stack aState)) (regs aState),
                    stack = drop 2 (stack aState)
                  }
  checker

neg ::  MSM Int
neg  = do
  x <- pop
  push (x * (-1))
  return (x * (-1))

add :: MSM Int
add = do
  x <- pop
  y <- pop
  push (x + y)
  return (x + y)

jmp :: MSM ()
jmp = do
  x <- pop
  aState <- get
  set (aState {pc = x})

cjmp :: Int -> MSM ()
cjmp a = do
  x <- pop
  aState <- get
  if x > 0
  then do set (aState {pc = pc aState +1 })
  else do set (aState {pc = a})
  

decodeError :: Error -> String
decodeError anError = case (errorType anError) of
  InvalidPC -> "Invalid PC"
  StackUnderflow -> "StackUnderflow"
  RegisterAlreadyAllocated -> "Register already registered"
  UnallocatedRegister a -> "UnallocatedRegister " ++ show a


---- | Run the given program on the MSM
runMSM :: Prog -> Either String State
runMSM p = let (MSM f) = interp
           in fmap snd $ f $ initial p

--Example program, when it terminates it leaves 42 on the top of the stack
p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]

