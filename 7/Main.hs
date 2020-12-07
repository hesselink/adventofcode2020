import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Map (Map)
import Data.Void (Void)
import Data.List (sort, nub)
import Control.Arrow (second)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/7"
  let containsMap = parseContains f
      containedMap = invert containsMap
      result = length (contained containedMap "shiny gold")
  print result
  let result2 = contains containsMap "shiny gold"
  print result2

type BagContains = Map String [(String, Int)]
type BagContained = Map String [String]

contains :: BagContains -> String -> Int
contains bc = sum . map snd . contains_ bc

contains_ :: BagContains -> String -> [(String, Int)]
contains_ bc b =
  let direct = Map.findWithDefault [] b bc
  in direct ++ concatMap (\(ib, n) -> map (second (*n)) (contains_ bc ib)) direct

contained :: BagContained -> String -> [String]
contained bc b =
  let direct = Map.findWithDefault [] b bc
  in nub . sort $ direct ++ concatMap (contained bc) direct

invert :: BagContains -> BagContained
invert = Map.foldrWithKey (\k vs m -> foldr (\(v, _) -> Map.insertWith (<>) v [k]) m vs) Map.empty

parseContains :: String -> BagContains
parseContains = either (error . errorBundlePretty) id . runParser pRules "input"

type P = Parsec Void String

pRules :: P BagContains
pRules = Map.fromList <$> (pLine `endBy` newline) <* eof

pLine :: P (String, [(String, Int)])
pLine = (,) <$> pBag <* string " contain " <*> pNumBags <*  char '.'

pNumBags :: P [(String, Int)]
pNumBags =  [] <$ string "no other bags" <|> (pNumBag `sepBy` string ", ")

pNumBag :: P (String, Int)
pNumBag = flip (,) <$> pNumber <* space <*> pBag

pBag :: P String
pBag = bag <|> append <$> some letterChar <*> space <*> pBag
  where
    bag = "" <$ string "bag" <* optional (char 's')
    append w () b = w ++ if null b then "" else " " ++ b

pNumber :: P Int
pNumber = read <$> some digitChar
