{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Set (Set)
import Data.Maybe (catMaybes)
import Data.List (sort, nub)
import qualified Data.Set as Set

main :: IO ()
main = do
  f <- readFile "input/17"
  let g = parseGrid f
      g' = iterate step (toGrid3 g) !! 6
  print (Set.size g')
  let g'' = iterate step (toGrid4 g) !! 6
  print (Set.size g'')

type Grid p = Set p -- actives
type Grid2 = Grid Pos2
type Grid3 = Grid Pos3
type Grid4 = Grid Pos4
type Pos2 = (Int, Int) -- (x, y)
type Pos3 = (Int, Int, Int) -- (x, y, z)
type Pos4 = (Int, Int, Int, Int) -- (x, y, z, w)

step :: (Ord p, Pos p) => Grid p -> Grid p
step g =
  let poss = Set.toList g
      neighbors = filter (not . (`Set.member` g)) . nub . sort . concatMap surrounding $ poss
      newPoss = filter (remainActive g) poss
      newNeighbors = filter (becomeActive g) neighbors
  in Set.fromList (newPoss ++ newNeighbors)

remainActive :: (Ord p, Pos p) => Grid p -> p -> Bool
remainActive g p = countActiveNeighbors g p `elem` [2,3]

becomeActive :: (Ord p, Pos p) => Grid p -> p -> Bool
becomeActive g p = countActiveNeighbors g p == 3

parseGrid :: String -> Grid2
parseGrid
  = Set.fromList
  . catMaybes
  . concatMap (\(y, l) -> zipWith (\x c -> if c == '#' then Just (x, y) else Nothing) [0..] l)
  . zip [0.. ]
  . lines

countActiveNeighbors :: (Ord p, Pos p) => Grid p -> p -> Int
countActiveNeighbors g = count (flip Set.member g) . surrounding

class Pos p where
  surrounding :: p -> [p]

instance Pos Pos3 where
  surrounding (x, y, z) =
    [ (x', y', z')
    | x' <- [x - 1, x, x + 1]
    , y' <- [y - 1, y, y + 1]
    , z' <- [z - 1, z, z + 1]
    , (x, y, z) /= (x', y', z')
    ]

instance Pos Pos4 where
  surrounding (x, y, z, w) =
    [ (x', y', z', w')
    | x' <- [x - 1, x, x + 1]
    , y' <- [y - 1, y, y + 1]
    , z' <- [z - 1, z, z + 1]
    , w' <- [w - 1, w, w + 1]
    , (x, y, z,w) /= (x', y', z', w')
    ]

count :: (a -> Bool) -> [a] -> Int
count p = length . filter id . map p

toGrid3 :: Grid2 -> Grid3
toGrid3 = Set.mapMonotonic (\(x, y) -> (x, y, 0))

toGrid4 :: Grid2 -> Grid4
toGrid4 = Set.mapMonotonic (\(x, y) -> (x, y, 0, 0))
