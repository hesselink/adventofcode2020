{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Arrow ((***))
import Control.Monad (when, forM_)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, runStateT, get, put, modify, MonadState)
import Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError)
import Data.Map (Map)
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  f <- readFile "input/8"
  let pr = parseProgram f
      st = runInterpreter run pr
  print (accumulator . snd $ st)
  let prs = modifications pr
      sts = map (runInterpreter run) $ prs
      success = head . dropWhile (isLeft . fst) $ sts
  print (accumulator . snd $ success)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

modifications :: Program -> [Program]
modifications p =
  mapMaybe (modifyOne p) (Map.keys p)

modifyOne :: Program -> Address -> Maybe Program
modifyOne p a = do
  instr <- Map.lookup a p
  case instr of
    Instr Nop n | n /= Arg 0 && n /= Arg 1 -> Just $ Map.insert a (Instr Jmp n) p
    Instr Jmp n | n /= Arg 1 -> Just $ Map.insert a (Instr Nop n) p
    _ -> Nothing

-- Interpreter types

newtype Address = Address Int
  deriving (Show, Eq, Ord)

incAddress :: Address -> Address
incAddress (Address n) = Address (n + 1)

addAddress :: Int -> Address -> Address
addAddress n (Address a) = Address (a + n)

data InterpreterState = InterpreterState
  { program :: Program
  , address :: Address
  , seenAddresses :: Set Address
  , accumulator :: Int
  } deriving (Show, Eq)

data InterpreterError = Loop

newtype InterpreterT m a = InterpreterT { unInterpreter :: ExceptT InterpreterError (StateT InterpreterState m) a }
  deriving (Functor, Applicative, Monad, MonadState InterpreterState, MonadError InterpreterError)

type Interpreter = InterpreterT Identity

runInterpreter :: Interpreter a -> Program -> (Either InterpreterError a, InterpreterState)
runInterpreter i p = runIdentity $ runStateT (runExceptT (unInterpreter i)) (InterpreterState p (Address 0) mempty 0)

-- Running

run :: Interpreter ()
run = do
  checkLoop
  mInstr <- popInstr
  forM_ mInstr $ \instr -> do
    runInstr instr
    run

checkLoop :: Interpreter ()
checkLoop = do
  st <- get
  when (address st `Set.member` seenAddresses st) $
    throwError Loop

runInstr :: Instr -> Interpreter ()
runInstr = \case
  Instr Acc (Arg x) -> modify $ \st -> st { accumulator = accumulator st + x }
  Instr Jmp (Arg x) -> modify $ \st -> st { address = addAddress (x - 1) (address st) } -- subtract one to compensate for the increase on every instruction
  Instr Nop _ -> return ()

popInstr :: Interpreter (Maybe Instr)
popInstr = do
  st <- get
  let addr = address st
      mInstr = Map.lookup addr (program st)
  when (isJust mInstr) $ put st { address = incAddress addr, seenAddresses = Set.insert addr (seenAddresses st) }
  return mInstr

-- Program types

data OpCode = Acc | Jmp | Nop
  deriving (Show, Eq)

data Instr = Instr OpCode Arg
  deriving (Show, Eq)

newtype Arg = Arg Int
  deriving (Show, Eq)

type Program = Map Address Instr

-- Parsing code

parseProgram :: String -> Program
parseProgram = Map.fromList . zip (map Address [0..]) . map parseInstr . lines

parseInstr :: String -> Instr
parseInstr = uncurry Instr . (parseOpCode *** (parseArg . tail)) . break (== ' ')

parseOpCode :: String -> OpCode
parseOpCode = \case
  "acc" -> Acc
  "jmp" -> Jmp
  "nop" -> Nop
  c     -> error $ "Unknown opcode: " ++ c

parseArg :: String -> Arg
parseArg "" = error "Empty string in parseArg"
parseArg ('+':a) = Arg (read a)
parseArg a = Arg (read a)
