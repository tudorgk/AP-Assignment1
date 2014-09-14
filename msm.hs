module MSM where

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
newtype MSM a = MSM (State -> Either String (a, State))

instance Monad MSM where
    -- return :: a -> MSM a
    return a = MSM (\x -> Right (a,x))
    
    -- fail :: String -> MSM a
    fail s = MSM (\x -> Left s)
    
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    -- trying to write it like this so I can UNDERSTAND!!
    (MSM rightOrLeft) >>= appliedFunction = MSM (\state -> case rightOrLeft state of
                            Left aState -> Left aState
                            Right aState -> let Right (value, state2) = rightOrLeft state;
                                                (MSM p2) = appliedFunction value
                                                in p2 state2
                            )    

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
  --traceMonad currentState -- show the state for debugging
  case inst of 
    PUSH a -> do 
      push a currentState
    POP -> do 
      pop currentState
    DUP -> do
      dup currentState
    SWAP -> do
      swap currentState
    NEWREG a -> do
      newreg a currentState
    LOAD -> do
      load currentState
    STORE -> do
      store currentState
    NEG -> do
      neg currentState
    ADD -> do
      add currentState
    JMP -> do
      jmp currentState
    CJMP a -> do
      cjmp a currentState
    HALT -> do -- Stop! Cease and desist!
      return False
    


push :: Int -> State -> MSM Bool  
push a aState = do
  set aState{stack = a:stack aState, pc = pc aState + 1}
  return True

pop :: State -> MSM Bool -- how do I return Int if cont expects a Bool value??
pop aState = do 
  let (first:others) = stack aState
  let checker | List.null (stack aState) = fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = set aState{stack = others, pc = pc aState + 1 }  
  checker
  return True

dup :: State -> MSM Bool
dup aState = do
  let checker | List.null (stack aState) = fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = set aState{stack = head(stack aState) : stack aState, pc = pc aState + 1 }
  checker
  return True 

swapStack::Stack -> Stack
swapStack stackToSwap = 
  let (first, second) = (head stackToSwap, head $ tail stackToSwap) in
  second : first : drop 2 stackToSwap


swap :: State -> MSM Bool
swap aState = do
  let checker | length  (stack aState) < 2 = fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = set aState{stack = swapStack (stack aState), pc = pc aState + 1} 
  checker
  return True

newreg :: Int -> State -> MSM Bool
newreg aReg aState = do
  let checker | Map.member aReg (regs aState) = fail $ decodeError Error{errorType = RegisterAlreadyAllocated}
              | otherwise = set aState{regs = Map.insert aReg 0 (regs aState), pc = pc aState + 1 }
  checker
  return True

load :: State -> MSM Bool
load aState = do
  let checker | List.null (stack aState) = fail $ decodeError Error{errorType = StackUnderflow}
              | not (Map.member (head (stack aState)) (regs aState)) = fail $ decodeError Error{errorType = UnallocatedRegister (head (stack aState)) }
              | otherwise = set aState{stack = regs aState Map.! head (stack aState) : tail (stack aState), pc = pc aState + 1}
  checker
  return True

store :: State -> MSM Bool
store aState = do
  let checker | not (Map.member (stack aState !! 1) (regs aState)) = fail $ decodeError Error{errorType = UnallocatedRegister (stack aState !! 1) }   
              | length (stack aState) < 2 = fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = set aState{regs = Map.insert (stack aState !! 1) (head (stack aState)) (regs aState), 
                                                                        stack = drop 2 (stack aState), pc = pc aState + 1}
  checker
  return True

neg :: State -> MSM Bool
neg aState = do
  let checker | List.null (stack aState) = fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = set aState{stack = head(stack aState)*(-1) : tail(stack aState), pc = pc aState + 1 } 
  checker      
  return True

add :: State -> MSM Bool
add aState = do
  let checker | length (stack aState) < 2 = fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = set aState{stack = head(stack aState) + head(tail(stack aState)) : drop 2 (stack aState), pc = pc aState + 1 } 
  checker
  return True

jmp :: State -> MSM Bool
jmp aState = do
  let checker | List.null (stack aState) = fail $ decodeError Error{errorType = StackUnderflow}
              | otherwise = set aState{pc = head(stack aState), stack = tail(stack aState) }  
  checker
  return True

cjmp :: Int -> State -> MSM Bool
cjmp a aState = do
  let checker | List.null (stack aState) = fail $ decodeError Error{errorType = StackUnderflow}
              | head (stack aState) < 0 = set aState{stack = tail (stack aState), pc = a}
              | otherwise = set aState{stack = tail (stack aState), pc = pc aState + 1}
  checker
  return True

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
p21 = [NEWREG 1, PUSH 0, PUSH 1,  DUP, NEG,SWAP,LOAD,HALT]
