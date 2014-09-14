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
  stat <- get -- get the state
  if pc stat > length (prog stat) || pc stat < 0 -- check pc from state bounds
    then fail (decodeError Error {errorType = InvalidPC}) -- respond with an error
    else return $ prog stat !! pc stat-- return the inst at the PC index from the Prog list

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
interpInst inst | Trace.trace("called interpinst with inst "++ show inst) False = undefined
interpInst inst = do
  currentState <- get
  case inst of 
    PUSH a -> do 
        push a currentState
    POP -> do 
        pop currentState
    


push :: Int -> State -> MSM Bool  
push a aState | Trace.trace ("push called with state" ++ show aState) False = undefined
push a aState = do
  set aState{stack = a:stack aState, pc = pc aState + 1}
  return True

pop :: State -> MSM Bool -- how do I return Int if cont expects a Bool value??
pop aState = do 
  let (first:others) = stack aState
  if List.null (stack aState)
    then fail (decodeError Error{errorType = StackUnderflow})
    else set aState{stack = others, pc = pc aState + 1 }  
  return True


dup :: State -> MSM Bool
dup aState = do
  if List.null (stack aState) 
    then fail (decodeError Error{errorType = StackUnderflow})
    else set aState{stack = head(stack aState) : stack aState, pc = pc aState + 1 }
  return True 

decodeError :: Error -> String
decodeError anError = case (errorType anError) of
  InvalidPC -> "Invalid PC"
  StackUnderflow -> "StackUnderflow"
  
---- | Run the given program on the MSM
runMSM :: Prog -> Either String State
runMSM p = let (MSM f) = interp
           in fmap snd $ f $ initial p

  
--Example program, when it terminates it leaves 42 on the top of the stack
--p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]
p21 = [PUSH 0, PUSH 1]
