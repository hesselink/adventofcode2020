{-# LANGUAGE NumericUnderscores #-}
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/15"
  let ns = map read . splitOn "," $ f
      initialState = State
        { turn = length ns
        , lastSeen = Map.fromList (zip (init ns) [1..])
        , lastNum = last ns
        }
      allNums = (init ns ++) . map lastNum . iterate step $ initialState
      result = allNums !! 2019
  print result
  let result2 = allNums !! 29_999_999
  print result2

data State = State
  { turn :: Int
  , lastSeen :: Map Int Int
  , lastNum :: Int
  } deriving (Show, Eq)

step :: State -> State
step s =
  case Map.lookup (lastNum s) (lastSeen s) of
    Nothing -> State
      { turn = turn s + 1
      , lastSeen = Map.insert (lastNum s) (turn s) (lastSeen s)
      , lastNum = 0
      }
    Just seen -> State
      { turn = turn s + 1
      , lastSeen = Map.insert (lastNum s) (turn s) (lastSeen s)
      , lastNum = turn s - seen
      }
