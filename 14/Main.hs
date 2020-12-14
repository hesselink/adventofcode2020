import Control.Monad (zipWithM)
import Data.Bits ((.&.), (.|.))
import Data.List (isPrefixOf, foldl')
import Data.Map (Map)
import Numeric (readInt, showIntAtBase)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/14"
  let p = map parseInstr . lines $ f
      finalState = foldl' (flip runInstr) initialState p
      res = sum (mem finalState)
  print res
  let finalState2 = foldl' (flip runInstr2) initialState p
      res2 = sum (mem finalState2)
  print res2

type Program = [Instr]
type Addr = Int
type Val = Int
data Instr
  = SetMem Addr Val
  | Mask String
  deriving (Show, Eq)

parseInstr :: String -> Instr
parseInstr s =
  if maskPrefix `isPrefixOf` s
  then Mask (drop (length maskPrefix) s)
  else
    let addr = read . takeWhile (/= ']') . drop (length memPrefix) $ s
        val = read . drop (length "] = ") . dropWhile (/= ']') . drop (length memPrefix) $ s
    in SetMem addr val
  where
    maskPrefix = "mask = "
    memPrefix = "mem["

type Mask = Int
data State = State
  { mem :: Map Addr Val
  , mask :: String
  } deriving (Show, Eq)

initialState :: State
initialState = State mempty ""

runInstr :: Instr -> State -> State
runInstr (SetMem a v) s =
  let (zeroMask, oneMask) = parseMask (mask s)
  in s { mem = Map.insert a (v .&. zeroMask .|. oneMask) (mem s) }
runInstr (Mask str) s = s { mask = str }

parseMask :: String -> (Mask, Mask)
parseMask str =
  ( parseBin $ map (\c -> if c == 'X' then '1' else c) str
  , parseBin $ map (\c -> if c == 'X' then '0' else c) str
  )

parseBin :: String -> Int
parseBin = fst . head . readInt 2 (`elem` "01") (read . pure)

runInstr2 :: Instr -> State -> State
runInstr2 (SetMem a v) s =
  s { mem = foldl' (\m a'  -> Map.insert a' v m) (mem s) (applyMask a (mask s)) }
runInstr2 (Mask str) s = s { mask = str }

applyMask :: Addr -> String -> [Addr]
applyMask a str = map parseBin $ zipWithM applyBit (pad 36 '0' $ showBin a) str

applyBit :: Char -> Char -> String
applyBit a m =
  case m of
    '0' -> pure a
    '1' -> "1"
    'X' -> "01"
    _ -> error $ "illegal mask char: " ++ [m]

showBin :: Int -> String
showBin n = showIntAtBase 2 (head . show) n ""

pad :: Int -> a -> [a] -> [a]
pad n x xs = replicate (n - length xs) x ++ xs
