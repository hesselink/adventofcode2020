{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((&&&))
import Data.Map (Map)
import Data.Maybe (mapMaybe, isJust)
import Data.List (groupBy)
import Data.Function (on)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/11"
  let g = parseGrid f
      result = countOccupied (stepUntilStable g)
  print result
  let result2 = countOccupied (stepUntilStable2 g)
  print result2

type Grid = Map Pos Tile
type Visible = Map Pos [Pos]
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
step g = stepWith (getSurrounding g) 4 g

step2 :: Visible -> Grid -> Grid
step2 v g = stepWith (getVisible v g) 5 g

stepWith :: (Pos -> [Tile]) -> Int -> Grid -> Grid
stepWith getAffecting maxNum g = Map.mapWithKey nextState g
  where
    nextState p = \case
      Empty -> if any (== Occupied) (getAffecting p)
               then Empty
               else Occupied
      Occupied -> if (>= maxNum) . length . filter (== Occupied) . getAffecting $ p
                  then Empty
                  else Occupied
      t -> t

stepUntilStable :: Grid -> Grid
stepUntilStable = stepUntilStableWith step

stepUntilStable2 :: Grid -> Grid
stepUntilStable2 g =
  let v = allVisible g
  in stepUntilStableWith (step2 v) g

stepUntilStableWith :: (Grid -> Grid) -> Grid -> Grid
stepUntilStableWith stepper grid =
  let gs = iterate stepper grid
  in fst . head . filter (uncurry (==)) . zip (drop 1 gs) $ gs

countOccupied :: Grid -> Int
countOccupied = length . filter (== Occupied) . Map.elems

getSurrounding :: Grid -> Pos -> [Tile]
getSurrounding g p = mapMaybe (flip Map.lookup g) . surrounding $ p

surrounding :: Pos -> [Pos]
surrounding (y, x) = [(y', x') | y' <- [y - 1, y, y + 1], x' <- [x - 1, x, x + 1], (y, x) /= (y', x')]

getVisible :: Visible -> Grid -> Pos -> [Tile]
getVisible v g p = mapMaybe (flip Map.lookup g) $ Map.findWithDefault [] p v

allVisible :: Grid -> Visible
allVisible g
  = Map.fromList
  . map ((id &&& visible g) . fst)
  . filter ((/= Floor) . snd)
  . Map.toList
  $ g

visible :: Grid -> Pos -> [Pos]
visible g p = mapMaybe (firstVisible p g)
  [ (dy, dx) | dy <- [-1, 0, 1], dx <- [-1, 0, 1], dx /= 0 || dy /= 0 ]

firstVisible :: Pos -> Grid -> (Int, Int) -> Maybe Pos
firstVisible p g d
  = (\(mt, p') -> if isJust mt then Just p' else Nothing)
  . head
  . dropWhile ((== Just Floor) . fst)
  . map (flip Map.lookup g &&& id)
  . drop 1
  . iterate (translate d)
  $ p

translate :: (Int, Int) -> Pos -> Pos
translate (dy, dx) (y, x) = (y + dy, x + dx)

printGrid :: Grid -> String
printGrid = unlines . map (map (printTile . snd)) . groupBy ((==) `on` fst . fst) . Map.toAscList

printTile :: Tile -> Char
printTile = \case
  Floor -> '.'
  Empty -> 'L'
  Occupied -> '#'
