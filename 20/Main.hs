import Data.List.Split (splitOn)

main :: IO ()
main = do
  f <- readFile "input/20"
  let ts = parseTiles f
  print ts
  return ()

data Tile = Tile
  { id :: Int
  , grid :: Grid
  } deriving (Show, Eq)

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
