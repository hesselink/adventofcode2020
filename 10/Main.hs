import Data.List (sort, group)
import Data.Function (fix)

main :: IO ()
main = do
  f <- readFile "input/10"
  let input = sort . map read . lines $ f
      vs = input ++ [last input + 3]
      diffs = zipWith subtract (0:vs) vs
      [ones, threes] = map length . group . sort . filter (`elem` [1,3]) $ diffs
  print (ones * threes)
  let result2 = countPaths (last vs) vs
  print result2

countPaths :: Int -> [Int] -> Int
countPaths end vs = fix (memoize . go) 0
  where
    go _   n | n == end = 1
    go rec n = sum . map rec . takeWhile (\x -> x - n <= 3) . dropWhile (<= n) $ vs

memoize :: (Int -> a) -> Int -> a
memoize f = (map f [0..] !!)
