import Data.List (foldl', intersect, intercalate)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set ((\\))

main :: IO ()
main = do
  f <- readFile "input/21"
  let ls = map parseLine . lines $ f
      allergenOptions = makeAllergenMap ls
      minimalAllergenOptions = minimizeOptions allergenOptions
      allIngredients = concatMap fst ls
      safeIngredients = Set.fromList allIngredients \\ Set.fromList (concat (Map.elems minimalAllergenOptions))
      safeIngredientCount = length $ filter (`Set.member` safeIngredients) allIngredients
  print safeIngredientCount
  let dangerousIngredientList = intercalate "," . concatMap (map unIngredient . snd) . Map.toAscList $ minimalAllergenOptions
  print dangerousIngredientList

newtype Ingredient = Ingredient { unIngredient :: String }
  deriving (Show, Eq, Ord)
newtype Allergen = Allergen String
  deriving (Show, Eq, Ord)

parseLine :: String -> ([Ingredient], [Allergen])
parseLine line =
  let (iStr, rest) = break (== '(') line
  in (map Ingredient . splitOn " " . init $ iStr, map Allergen . splitOn ", " . init . drop 10 $ rest)

makeAllergenMap :: [([Ingredient], [Allergen])] -> Map Allergen [Ingredient]
makeAllergenMap = foldl' (\m (is, as) -> foldl' (\m' a -> Map.insertWith combineOptions a is m') m as) Map.empty

combineOptions :: [Ingredient] -> [Ingredient] -> [Ingredient]
combineOptions = intersect

minimizeOptions :: Map Allergen [Ingredient] -> Map Allergen [Ingredient]
minimizeOptions m =
  let uniques = mapMaybe fromSingleton . Map.elems $ m
      m' = fmap (removeAll uniques) m
  in if m == m'
     then m
     else minimizeOptions m'

fromSingleton :: [a] -> Maybe a
fromSingleton [x] = Just x
fromSingleton _ = Nothing

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll _ [x] = [x]
removeAll toRemove xs = filter (not . (`elem` toRemove)) xs
