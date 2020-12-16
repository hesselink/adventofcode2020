import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Map (Map)
import Data.Void (Void)
import Data.List (intersect, isPrefixOf)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/16"
  let input = parseInput f
      result = sum (invalidFieldsInNearbyTickets input)
  print result
  let validTickets = filter (all (matchesAny (Map.elems . rules $ input))) (otherTickets input)
      validFields = foldr1 (zipWith intersect) . map (map (validFor (rules input))) $ validTickets
      fieldOrder = findOrder validFields
      result2 = product . map snd . filter (("departure" `isPrefixOf`) . fst) . zip fieldOrder . ticket $ input
  print result2

data Input = Input
  { rules :: Map String Rule
  , ticket :: Ticket
  , otherTickets :: [Ticket]
  } deriving (Show, Eq)

data Rule = Rule
  { range1 :: (Int, Int)
  , range2 :: (Int, Int)
  } deriving (Show, Eq)

type Ticket = [Int]

findOrder :: [[String]] -> [String]
findOrder fs =
  let unique = map head . filter ((== 1) . length) $ fs
      newFs = map (\f -> if length f == 1 then f else filter (not . (`elem` unique)) f) fs
  in if length unique == length fs
     then unique
     else findOrder newFs

validFor :: Map String Rule -> Int -> [String]
validFor rs i = map fst . filter (matchesRule i . snd) . Map.toList $ rs

invalidFieldsInNearbyTickets :: Input -> [Int]
invalidFieldsInNearbyTickets input =
  filter (not . matchesAny (Map.elems . rules $ input)) . concat . otherTickets $ input

matchesAny :: [Rule] -> Int -> Bool
matchesAny rs i = any (matchesRule i) rs

matchesRule :: Int -> Rule -> Bool
matchesRule i (Rule (f1, t1) (f2, t2)) = i >= f1 && i <= t1 || i >= f2 && i <= t2

parseInput :: String -> Input
parseInput = either (error . errorBundlePretty) id . runParser pInput "input"

type P = Parsec Void String

pInput :: P Input
pInput = Input <$> pRules <* newline <*> pMyTicket <* newline <*> pNearbyTickets <* eof

pRules :: P (Map String Rule)
pRules = Map.fromList <$> (try pRule `endBy` newline)

pRule :: P (String, Rule)
pRule = (,) <$> some (letterChar <|> spaceChar) <* string ": " <*> (Rule <$> pRange <* string " or " <*> pRange)

pRange :: P (Int, Int)
pRange = (,) <$> pNumber <* char '-' <*> pNumber

pNumber :: P Int
pNumber = read <$> some digitChar

pMyTicket :: P Ticket
pMyTicket = string "your ticket:" <* newline *> pTicket <* newline

pTicket :: P Ticket
pTicket = pNumber `sepBy` char ','

pNearbyTickets :: P [Ticket]
pNearbyTickets = string "nearby tickets:" <* newline *> pTicket `endBy` newline
