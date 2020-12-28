import Data.Maybe
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/20"
  let ts = parseTiles f
      solutions = fitAll ts
      result = checksum . head $ solutions
      checksum s =
        let t = head s
            b = last s
        in id_ (head t) * id_ (last t) * id_ (head b) * id_ (last b)
  print result

data Tile = Tile
  { id_ :: Int
  , grid :: Grid
  } deriving (Show, Eq, Ord)

data Grid = Grid
  { top :: Edge
  , right :: Edge
  , bottom :: Edge
  , left :: Edge
  } deriving (Show, Eq, Ord)

type Edge = [Bool]

parseTiles :: String -> [Tile]
parseTiles = map parseTile . splitOn [""] . lines

parseTile :: [String] -> Tile
parseTile (x:xs) = Tile (parseHeader x) (parseGrid xs)
parseTile [] = error "Empty tile"

parseHeader :: String -> Int
parseHeader = read . takeWhile (/= ':') . drop 5

parseGrid :: [String] -> Grid
parseGrid = mkGrid . map (map (== '#'))

mkGrid :: [[Bool]] -> Grid
mkGrid bs = Grid (head bs) (map last bs) (last bs) (map (head) bs)

rotateR :: Grid -> Grid
rotateR (Grid t r b l) = Grid (reverse l) t (reverse r) b

flipH :: Grid -> Grid
flipH (Grid t r b l) = Grid (reverse t) l (reverse b) r

flipV :: Grid -> Grid
flipV (Grid t r b l) = Grid b (reverse r) t (reverse l)

flipsAndRotations :: Tile -> [Tile]
flipsAndRotations (Tile i g) = map (Tile i)
  [ g
  , rotateR g
  , rotateR . rotateR $ g
  , rotateR . rotateR . rotateR $ g
  , flipH g
  , flipV g
  , rotateR . flipH $ g
  , rotateR . flipV $ g
  ]

-- does t1 fit to the right of t2
fitRight :: Tile -> Tile -> Bool
fitRight t1 t2 = fitRightG (grid t1) (grid t2)

-- does t1 fit at the bottom of t2
fitBottom :: Tile -> Tile -> Bool
fitBottom t1 t2 = fitBottomG (grid t1) (grid t2)

-- does g1 fit to the right of g2
fitRightG :: Grid -> Grid -> Bool
fitRightG g1 g2 = right g2 == left g1

-- does g1 fit at the top of g2
fitBottomG :: Grid -> Grid -> Bool
fitBottomG g1 g2 = bottom g2 == top g1

type Solution = [[Tile]]

fitAll :: [Tile] -> [Solution]
fitAll ts =
  let side = round . sqrt . fromIntegral . length $ ts
  in map (toSolution side) $ fitPartial side (0,0) Map.empty ts

fitPartial :: Int -> (Int, Int) -> Map (Int, Int) Tile -> [Tile] -> [Map (Int, Int) Tile]
fitPartial side (curX, curY) pickedTiles availTiles =
  if curX >= side || curY >= side
  then [pickedTiles]
  else
    let nextX = if curX + 1 == side then 0 else curX + 1
        nextY = if nextX == 0 then curY + 1 else curY
        topTile = Map.lookup (curX, curY - 1) pickedTiles
        leftTile = Map.lookup (curX - 1, curY) pickedTiles
        isFit t = maybe True (fitRight t) leftTile && maybe True (fitBottom t) topTile
        possibleTiles = filter isFit . concatMap flipsAndRotations $ availTiles -- TODO optimize?
    in concatMap (\t -> fitPartial side (nextX, nextY) (Map.insert (curX, curY) t pickedTiles) (filter (\t' -> id_ t /= id_ t') availTiles)) possibleTiles

toSolution :: Int -> Map (Int, Int) Tile -> Solution
toSolution side m = map (\y -> mapMaybe (\x -> Map.lookup (x, y) m) [0..side-1]) [0..side-1]
