import Data.List.Split (splitOn)
import Data.List (sortBy, find, foldl')
import Control.Arrow ((&&&), second)
import Data.Ord (comparing)

main :: IO ()
main = do
  f <- readFile "input/13"
  let [tStr,busStr] = lines f
      t = read tStr
      busses = map read . filter (/= "x") . splitOn "," $ busStr
      result = (\(totalT, busId) -> (totalT - t) * busId)
        . head
        . sortBy (comparing fst)
        . map (firstMultipleLargerThan t &&& id)
        $ busses
  print result
  let ((_, n):bussesWithIx) =  map (second read) . filter ((/= "x") . snd) . zip [0..] . splitOn "," $ busStr
      s = foldl' next (State n n) bussesWithIx
  print (tCur s)

firstMultipleLargerThan :: Integer -> Integer -> Integer
firstMultipleLargerThan n x = x * ceiling (fromIntegral n / fromIntegral x)

data State = State
  { multiple :: Integer
  , tCur :: Integer
  } deriving (Show, Eq)

next :: State -> (Integer, Integer) -> State
next s (ix, n) =
  let Just nextT = find (\i -> i `mod` n == (-ix) `mod `n) [tCur s, tCur s + multiple s..]
      nextMult = lcm (multiple s) n
  in (State nextMult nextT)
