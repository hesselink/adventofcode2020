import Control.Arrow ((***))
import Data.List ((\\), sort)
import Numeric (readInt)

main :: IO ()
main = do
  f <- readFile "input/5"
  let passes = map parsePass . lines $ f
      maxSeatId = maximum . map seatId $ passes
  print maxSeatId
  let seat = findMissing passes
  print (seatId seat)

data Pass = Pass
  { row :: Int
  , column :: Int
  } deriving (Show, Eq, Ord)

findMissing :: [Pass] -> Pass
findMissing [] = error "No passes"
findMissing ps =
  let sortedPs = sort ps
      maxCol = maximum . map column $ ps
      allPasses = enum (head sortedPs) (last sortedPs) maxCol
  in head (allPasses \\ ps)

enum :: Pass -> Pass -> Int -> [Pass]
enum f t maxCol = [ p | r <- [row f..row t], c <- [0..maxCol], let p = Pass r c, p >= f, p <= t ]

seatId :: Pass -> Int
seatId p = row p * 8 + column p

parsePass :: String -> Pass
parsePass = uncurry Pass . (parseRow *** parseColumn) . splitAt 7

parseRow, parseColumn :: String -> Int
parseRow = parseBinary "BF" toZeroOne
  where
    toZeroOne 'B' = 1
    toZeroOne 'F' = 0
    toZeroOne c = error $ "illegal character: " ++ [c]
parseColumn = parseBinary "RL" toZeroOne
  where
    toZeroOne 'R' = 1
    toZeroOne 'L' = 0
    toZeroOne c = error $ "illegal character: " ++ [c]

parseBinary :: [Char] -> (Char -> Int) -> String -> Int
parseBinary elems toBit = fst . head . readInt 2 (`elem` elems) toBit
