{-# LANGUAGE NumericUnderscores #-}
import Data.List.Split (splitOn)
import Control.Monad (forM_)
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as V
import Control.Monad.ST (ST, runST)

main :: IO ()
main = do
  f <- readFile "input/15"
  let ns = map read . splitOn "," $ f
      result = calculate ns 2020
  print result
  let result2 = calculate ns 30_000_000
  print result2

data State s = State
  { turn :: Int
  , lastSeen :: LastSeen s
  , lastNum :: Int
  }

type LastSeen s = STVector s Int

calculate :: [Int] -> Int -> Int
calculate ns lastTurn =
  runST $ do
    initialSeen <- V.replicate 100000000 (-1)
    forM_ (zip (init ns) [1..]) $ \(n, t) ->
      addSeen n t initialSeen
    let initialState = State
          { turn = length ns
          , lastSeen = initialSeen
          , lastNum = last ns
          }
    lastNum <$> run lastTurn initialState

run :: Int -> State s -> ST s (State s)
run n = go
  where
    go s = do
      s' <- step s
      if turn s' == n
      then return s'
      else go s'

step :: State s -> ST s (State s)
step s = do
  mSeen <- getSeen (lastNum s) (lastSeen s)
  addSeen (lastNum s) (turn s) (lastSeen s)
  return $ case mSeen of
    Nothing -> State
      { turn = turn s + 1
      , lastSeen = lastSeen s
      , lastNum = 0
      }
    Just seen -> State
      { turn = turn s + 1
      , lastSeen = lastSeen s
      , lastNum = turn s - seen
      }
addSeen :: Int -> Int -> LastSeen s -> ST s ()
addSeen n t s = V.write s n t

getSeen :: Int -> LastSeen s -> ST s (Maybe Int)
getSeen n s = do
  t <- V.read s n
  return $ if t < 0
  then Nothing
  else Just t
