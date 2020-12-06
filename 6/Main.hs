import Data.List (sort, nub, intersect)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  f <- readFile "input/6"
  let gs = splitOn [""] . lines $ f
      cs = map countGroup gs
      result = sum cs
  print result
  let cs2 = map countGroup2 gs
      result2 = sum cs2
  print result2

countGroup :: [String] -> Int
countGroup = length . nub . sort . concat

countGroup2 :: [String] -> Int
countGroup2 = length . foldr1 intersect
