import Data.List (tails)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  f <- readFile "input/9"
  let ns = map read . lines $ f
      sums = mkSums ns
      result = snd . head . dropWhile (uncurry isValid) $ zip sums (drop preambleSize ns)
  print result
  let runs = map (drop 2 . scanl (+) 0) (tails ns)
      (start, len) = findSum result (zip [0..] runs)
      run = take (fromIntegral len) . drop (fromIntegral start) $ ns
      result2 = maximum run + minimum run
  print result2

preambleSize :: Int
preambleSize = 25

mkSums :: [Integer] -> [[Integer]]
mkSums [] = []
mkSums (x:xs) =
  let ss = map (x+) xs : mkSums xs
  in map takeSome (tails ss)

takeSome :: [[Integer]] -> [Integer]
takeSome xs = concatMap (uncurry take) . zip [preambleSize, preambleSize - 1 .. 1] $ xs

isValid :: [Integer] -> Integer -> Bool
isValid ss n = n `elem` ss

findSum :: Integer -> [(Integer, [Integer])] -> (Integer, Integer)
findSum n = head . mapMaybe (uncurry $ findSum_ n)

findSum_ :: Integer -> Integer -> [Integer] -> Maybe (Integer, Integer)
findSum_ n ix sums =
  let (s, ix2) = head (dropWhile ((< n) . fst) (zip sums [0..]))
  in if s == n then Just (ix, ix2 + 2) else Nothing
