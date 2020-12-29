{-# LANGUAGE TupleSections #-}
import Control.Arrow ((&&&))
import Data.List
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  f <- readFile "input/20"
  let ts = parseTiles f
      solution = head $ fitAll ts
      result = checksum solution
      checksum s =
        let t = head s
            b = last s
        in id_ (head t) * id_ (last t) * id_ (head b) * id_ (last b)
  print result
  let trimmedSolution = map (map (removeBorders . grid)) $ solution
      finalGrid = concatMap concatGrids trimmedSolution
      flips = flipsAndRotationsG finalGrid
      Just (ms, flippedGrid) = find (not . null . fst) . map ((\g -> matches g seaMonster) &&& id) $ flips
      cs = hashCoords flippedGrid
      baseMonsterCoords = hashCoords seaMonster
      allMonsterCoords = concatMap (\m -> map (shift m) baseMonsterCoords) ms
      notMonsterCoords = Set.fromList cs Set.\\ Set.fromList allMonsterCoords
      result2 = Set.size notMonsterCoords
  print result2

concatGrids :: [Grid] -> Grid
concatGrids = foldr1 concatGrid

concatGrid :: Grid -> Grid -> Grid
concatGrid = zipWith (++)

data Tile = Tile
  { id_ :: Int
  , grid :: Grid
  } deriving (Show, Eq, Ord)

type Grid = [[Bool]]

parseTiles :: String -> [Tile]
parseTiles = map parseTile . splitOn [""] . lines

parseTile :: [String] -> Tile
parseTile (x:xs) = Tile (parseHeader x) (parseGrid xs)
parseTile [] = error "Empty tile"

parseHeader :: String -> Int
parseHeader = read . takeWhile (/= ':') . drop 5

parseGrid :: [String] -> Grid
parseGrid = map (map (== '#'))

rotateR :: Grid -> Grid
rotateR = map reverse . transpose

flipH :: Grid -> Grid
flipH = reverse

flipV :: Grid -> Grid
flipV = map reverse

flipsAndRotations :: Tile -> [Tile]
flipsAndRotations (Tile i g) = map (Tile i) (flipsAndRotationsG g)

flipsAndRotationsG :: Grid -> [Grid]
flipsAndRotationsG g =
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
  where
    right = map last
    left = map head

-- does g1 fit at the top of g2
fitBottomG :: Grid -> Grid -> Bool
fitBottomG g1 g2 = bottom g2 == top g1
  where
    top = head
    bottom = last

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

removeBorders :: Grid -> Grid
removeBorders = map (init . tail) . init . tail

showGrid :: Grid -> String
showGrid = unlines . map (map (\b -> if b then '#' else '.'))

type Pattern = Grid

seaMonster :: Pattern
seaMonster = parseGrid $
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]

matches :: Grid -> Pattern -> [(Int, Int)]
matches = go 0
  where
    go p _ [] = [(0, p)]
    go _ [] _ = []
    go y (gl:gls) (pl:pls) =
      let ms = matchLine 0 gl pl
      in map (,y) (filter (continueMatches gls pls) ms) ++ go (y+1) gls (pl:pls)

continueMatches :: Grid -> Pattern -> Int -> Bool
continueMatches _ [] _ = True
continueMatches [] _ _ = False
continueMatches (gl:gls) (pl:pls) x =
  continueMatch (drop x gl) pl
  &&
  continueMatches gls pls x

matchLine :: Int -> [Bool] -> [Bool] -> [Int]
matchLine x _ [] = [x]
matchLine _ [] _ = []
matchLine x (g:gs) (p:ps) | match g p = (if continueMatch gs ps then (x:) else id) $ matchLine (x+1) gs (p:ps)
                          | otherwise = matchLine (x+1) gs (p:ps)

continueMatch :: [Bool] -> [Bool] -> Bool
continueMatch _ [] = True
continueMatch [] _ = False
continueMatch (g:gs) (p:ps) = match g p && continueMatch gs ps

match :: Bool -> Bool -> Bool
match _ False = True
match True True = True
match _ _ = False

hashCoords :: Grid -> [(Int, Int)]
hashCoords = concat . zipWith (\y -> mapMaybe (\(x, c) -> if c then Just (x, y) else Nothing)) [0..]  . map (zip [0..])

shift :: (Int, Int) -> (Int, Int) -> (Int, Int)
shift (dx, dy) (x, y) = (x + dx, y + dy)
