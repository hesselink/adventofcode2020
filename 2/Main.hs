import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Control.Arrow ((&&&), (***))
import Data.List (group, sort)
import Data.Maybe (mapMaybe, fromMaybe)

main :: IO ()
main = do
  f <- readFile "input/2"
  let inputs = mapMaybe parseInput . lines $ f
      result = length . filter isValid $ inputs
  print result
  let result2 = length . filter isValid2 $ inputs
  print result2

data Input = Input
  { constraint :: Constraint
  , password :: String
  } deriving (Show, Eq)

data Constraint = Constraint
  { range :: (Int, Int) -- smaller, larger
  , character :: Char
  } deriving (Show, Eq)

isValid :: Input -> Bool
isValid i
  = inRange
  . fromMaybe 0
  . lookup (character . constraint $ i)
  . map (head &&& length)
  . group
  . sort
  . password
  $ i
  where
    rng = range . constraint $ i
    inRange c = c >= fst rng && c <= snd rng

isValid2 :: Input -> Bool
isValid2 i = checkTwoPositions
  ((subtract 1 *** subtract 1) . range . constraint $ i)
  (character . constraint $ i)
  (password i)

checkTwoPositions :: (Int, Int) -> Char -> String -> Bool
checkTwoPositions (0, q) c (c':str) =  c == c' && not (checkOnePosition (q - 1) c str)
                                    || c /= c' && checkOnePosition (q - 1) c str
checkTwoPositions (p, q) c (_:str) = checkTwoPositions (p - 1, q - 1) c str
checkTwoPositions (_, _) _ [] = False

checkOnePosition :: Int -> Char -> String -> Bool
checkOnePosition 0 c (c':_) = c == c'
checkOnePosition n c (_:str) = checkOnePosition (n - 1) c str
checkOnePosition _ _ [] = False

parseInput :: String -> Maybe Input
parseInput str = do
  [conStr, pwd] <- pure $ splitOn ": " str
  con <- parseConstraint conStr
  return $ Input con pwd

parseConstraint :: String -> Maybe Constraint
parseConstraint str = do
  [rngStr, [chr]] <- pure $ splitOn " " str
  rng <- parseRange rngStr
  return $ Constraint rng chr

parseRange :: String -> Maybe (Int, Int)
parseRange str = do
  [startStr, endStr] <- pure $ splitOn "-" str
  start <- readMaybe startStr
  end <- readMaybe endStr
  return (start, end)
