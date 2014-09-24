-- Author: Tudor-Stefan Dragan

--DONE: Don’t let your modules export more than they have to.
module MSM (runMSM , Inst (..), CustomError (..) )where

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

data CustomError = StackUnderflow
               | UnallocatedRegister Int
               | RegisterAlreadyAllocated
               | InvalidPC
               | Unspec String
               deriving (Read, Eq)
--data Error = Error { errorType :: ErrorType}
--           deriving (Show, Eq)

{-TDOO: I was trying to do the 5th point from this link:
  http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/.
  And failed miserably :(. May you please guide me on how I should do the
  throwError? The version with the MonadError instance is in msm2.hs
-}

instance Show CustomError where
  show StackUnderflow = "Stack Underflow"
  show (UnallocatedRegister a) = "Unallocated register " ++ (show a)
  show RegisterAlreadyAllocated = "Register already allocated"
  show InvalidPC = "InvalidPC"
  show (Unspec str) = str

-- | `initial` constructs the initial state of an MSM running the
-- given program.
initial :: Prog -> State
initial p = State { prog = p
                  , pc = 0                     
                  , stack = []                 
                  , regs = Map.empty                      
                  }

-- | This is the monad that is used to implement the MSM. 
newtype MSM a = MSM { unMSM :: State -> Either String (a, State) }

instance Monad MSM where
    -- return :: a -> MSM a
    return a = MSM (\x -> Right (a,x))
    
    -- fail :: String -> MSM a
    fail s = MSM (\x -> Left s)
    
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    {-DONE: If you hadused record-style to define the newtype, one could avoid 
    the pattern matching and instead unwrap the 'MSM' constructor using its
    corresponding 'destructor':
    -}
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
    then fail  $ show InvalidPC-- respond with an error
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
    

{-DONE: Every monadic action with type 'State -> MSM sometype' should be changed
into 'MSM sometype'. In particular, push and newreg should probably be
'Int -> MSM Bool'... since you use them in interpInst.
-}
push :: Int -> MSM ()  
push a  = do
  aState <- get
  set aState{stack = a:stack aState}

pop ::  MSM Int -- how do I return Int if cont expects a Bool value??
pop  = do 
  aState <- get
  let (first:others) = stack aState
  let checker | List.null (stack aState) = fail $ show StackUnderflow
              | otherwise = set aState{stack = others}  
  checker
  return (head (stack aState)) 

{-DONE: I understood now how i should write these functions :)-}
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
                  fail $ show RegisterAlreadyAllocated
              | otherwise =
                  set aState { 
                    regs = Map.insert aReg 0 (regs aState)
                  }
  checker
  

load :: MSM ()
load = do
  aState <- get
  let checker | List.null (stack aState) = 
                  fail $ show StackUnderflow
              | not (Map.member (head (stack aState)) (regs aState)) = 
                  fail $ show $ UnallocatedRegister (head (stack aState)) 
              | otherwise = 
                  set aState{
                    stack = regs aState Map.! head (stack aState) : tail (stack aState)
                  }
  checker


store ::  MSM ()
store = do
  aState <- get
  let checker | not (Map.member (stack aState !! 1) (regs aState)) =
                  fail $ show $ UnallocatedRegister (stack aState !! 1)   
              | length (stack aState) < 2 = 
                  fail $ show StackUnderflow
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
  then return ()
  else set (aState {pc = a})


---- | Run the given program on the MSM
{-DONE: runMSM does not have the desired return value. It should return the topmost
element on the stack, not the complete machine state. When you make this kind
of choice, make sure you argue why. Otherwise it will be interpreted as not
being able to see the difference.-}
runMSM :: Prog -> Int
runMSM p = let 
            (MSM f) = interp
            x = fmap snd $ f $ initial p
           in case x of
            Left err -> error err
            Right state -> head $ stack state

{-DONE: Style:
- The convention for monadic variants of regular functions it to call them e.g. 'traceM'.
- Please keep a 80 or 100 character margin consistently. 150 chars is too long!-}