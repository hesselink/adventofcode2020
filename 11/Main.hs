{-# LANGUAGE LambdaCase #-}

import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.List (groupBy)
import Data.Function (on)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/11"
  let g = parseGrid f
      result = countOccupied (stepUntilStable g)
  print result

type Grid = Map Pos Tile
type Pos = (Int, Int) -- (y, x)
data Tile = Floor | Empty | Occupied
  deriving (Show, Eq)

parseGrid :: String -> Grid
parseGrid
  = Map.fromList
  . concatMap (\(l, cs) -> zipWith (\c t -> ((l, c), t)) [0..] (map parseTile cs))
  . zip [0..]
  . lines

parseTile :: Char -> Tile
parseTile = \case
  '.' -> Floor
  'L' -> Empty
  '#' -> Occupied
  c -> error $ "Unknown tile character: " ++ [c]

step :: Grid -> Grid
step g = Map.mapWithKey nextState g
  where
    nextState p = \case
      Empty -> if any (== Occupied) . getSurrounding g $ p
               then Empty
               else Occupied
      Occupied -> if (>= 4) . length . filter (== Occupied) . getSurrounding g $ p
                  then Empty
                  else Occupied
      t -> t

stepUntilStable :: Grid -> Grid
stepUntilStable grid =
  let gs = iterate step grid
  in fst . head . filter (uncurry (==)) . zip (drop 1 gs) $ gs

countOccupied :: Grid -> Int
countOccupied = length . filter (== Occupied) . Map.elems

getSurrounding :: Grid -> Pos -> [Tile]
getSurrounding g p = mapMaybe (flip Map.lookup g) . surrounding $ p

surrounding :: Pos -> [Pos]
surrounding (y, x) = [(y', x') | y' <- [y - 1, y, y + 1], x' <- [x - 1, x, x + 1], (y, x) /= (y', x')]

printGrid :: Grid -> String
printGrid = unlines . map (map (printTile . snd)) . groupBy ((==) `on` fst . fst) . Map.toAscList

printTile :: Tile -> Char
printTile = \case
  Floor -> '.'
  Empty -> 'L'
  Occupied -> '#'
