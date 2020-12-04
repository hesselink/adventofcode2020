import Data.Maybe (catMaybes)
import Data.List (unfoldr)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  f <- readFile "input/3"
  let grid = parseGrid f
      result = numTrees grid 3 1
  print result
  let result2 = product [ numTrees grid r d | (r, d) <- [(1,1), (3,1), (5,1), (7,1), (1,2)] ]
  print result2

data Grid = Grid
  { trees :: Set Pos
  , width :: Int
  , height :: Int
  } deriving (Show, Eq)

type Pos = (Int, Int)

parseGrid :: String -> Grid
parseGrid str =
  let ls = lines $ str
      ts = Set.fromList . concatMap (uncurry parseLine) . zip [0..] $ ls
      w  = length . head $ ls
      h  = length ls
  in Grid ts w h

parseLine :: Int -> String -> [Pos]
parseLine l = catMaybes . zipWith (\c v -> if v == '#' then Just (c, l) else Nothing) [0..]

makeSteps :: Int -> Int -> Int -> Int -> [Pos]
makeSteps w h right down = unfoldr next (0,0)
  where
    next (prevX, prevY) =
      let (newX, newY) = ((prevX + right) `mod` w, prevY + down)
      in if newY >= h
         then Nothing
         else Just ((newX, newY), (newX, newY))

numTrees :: Grid -> Int -> Int -> Int
numTrees grid r d =
  let steps = makeSteps (width grid) (height grid) r d
  in length $ filter (flip Set.member (trees grid)) steps
