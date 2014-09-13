module MSM where

-- we want to use monads here
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List as List

-- and you might also find Maps useful
import qualified Data.Map as Map

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
newtype MSM a = MSM (State -> Either Error (a, State))

instance Monad MSM where
    -- return :: a -> MSM a
    return a = MSM (\x -> Right (a,x))
    
    -- fail :: String -> MSM a
    fail s = MSM (\x -> Left Error{errorMessage = s, errorType = InvalidPC})
    
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    (MSM p) >>= k = MSM (\s -> case p s of
                            Right v -> let Right (r, state1) = p s;
                                               (MSM p1) = k r
                                         in p1 state1
                            Left v -> Left v
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
  if pc stat > length (prog stat) || (pc stat) < 0 -- check pc from state bounds
    then error "Invalid PC " -- respond with an error
    else return $ (prog stat) !! (pc stat) -- return the inst at the PC index from the Prog list

-- | This function runs the MSM.
interp :: MSM ()
interp = run
    where run = do inst <- getInst
                   cont <- interpInst inst
                   when cont run

-- | This function interprets the given instruction. It returns True
-- if the MSM is supposed to continue it's execution after this
-- instruction.
interpInst :: Inst -> MSM Bool
interpInst inst = do
  currentState <- get
  case inst of 
    PUSH a -> do 
        push a currentState
        return True


push :: Int -> State -> MSM ()
push a currentState = set currentState{stack = a:stack currentState, pc = pc currentState +1} 


---- | Run the given program on the MSM
--runMSM :: Prog -> ...
--runMSM p = let (MSM f) = interp 
--           in fmap snd $ f $ initial p



---- example program, when it terminates it leaves 42 on the top of the stack
--p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]

