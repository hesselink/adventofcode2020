import qualified Text.ParserCombinators.ReadP as P
import Data.List (group, sort)
import qualified Data.Set as Set
import Data.Set (Set, (\\))

main :: IO ()
main = do
  f <- readFile "input/24"
  let paths = map parsePath . lines $ f
      poss = map (walk (0,0)) paths
      result = length . filter isOdd . map length . group . sort $ poss
  print result
  let blackTiles = Set.fromList . map head . filter (isOdd . length) . group . sort $ poss
      finalTiles = iterate step blackTiles !! 100
  print (Set.size finalTiles)

type Path = [Dir]

data Dir = E | SE | SW | W | NW | NE
  deriving (Show, Eq, Ord, Bounded, Enum)

parsePath :: String -> Path
parsePath = fst . head . filter (null . snd) . P.readP_to_S pPath

pPath :: P.ReadP Path
pPath = P.many pDir

pDir :: P.ReadP Dir
pDir = P.choice
  [ E <$ P.string "e"
  , SE <$ P.string "se"
  , SW <$ P.string "sw"
  , W <$ P.string "w"
  , NW <$ P.string "nw"
  , NE <$ P.string "ne"
  ]

type Pos = (Int, Int) -- first component: E/W, second component: NW/SE. NE = (+1, -1), SW = (-1, +1)

walk :: Pos -> Path -> Pos
walk = foldr walk1
  where
    walk1 d (q, r) = case d of
      E -> (q+1, r)
      SE -> (q, r+1)
      SW -> (q-1, r+1)
      W -> (q-1, r)
      NW -> (q, r-1)
      NE -> (q+1, r-1)

isOdd :: Integral n => n -> Bool
isOdd n = n `mod` 2 == 1

adjacent :: Pos -> [Pos]
adjacent p = map (walk p . pure) [minBound .. maxBound]

flippedToWhile :: Set Pos -> Set Pos
flippedToWhile black = Set.filter (inRange . length . filter (`Set.member` black) . adjacent) $ black
  where
    inRange n = n == 0 || n > 2

flippedToBlack :: Set Pos -> Set Pos
flippedToBlack black =
  let whiteToCheck
        = Set.filter (not . (`Set.member` black))
        . Set.fromList
        . concatMap adjacent
        . Set.toList
        $ black
  in Set.filter ((== 2) . length . filter (`Set.member` black) . adjacent) $ whiteToCheck

step :: Set Pos -> Set Pos
step black =
  let newWhite = flippedToWhile black
      newBlack = flippedToBlack black
  in black `Set.union` newBlack \\ newWhite
