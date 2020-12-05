import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)
import Control.Arrow (second)
import Control.Applicative ((<|>))

main :: IO ()
main = do
  f <- readFile "input/4"
  let passports = parsePassports f
      result = length . filter isValid $ passports
  print result
  let result2 = length . filter isValid2 $ passports
  print result2

type Passport = [Field]
type Field = (String, String)

parsePassports :: String -> [Passport]
parsePassports
  = map parsePassport
  . splitOn [""]
  . lines

parsePassport :: [String] -> Passport
parsePassport = concatMap (map parseField . splitOn " ")

parseField :: String -> Field
parseField = second tail . break (== ':')

isValid :: Passport -> Bool
isValid p =
  let neededFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  in all (\f -> isJust (lookup f p)) neededFields

isValid2 :: Passport -> Bool
isValid2 p
  =  test isValidByr (lookup "byr" p)
  && test isValidIyr (lookup "iyr" p)
  && test isValidEyr (lookup "eyr" p)
  && test isValidHgt (lookup "hgt" p)
  && test isValidHcl (lookup "hcl" p)
  && test isValidEcl (lookup "ecl" p)
  && test isValidPid (lookup "pid" p)

test :: (String -> Bool) -> Maybe String -> Bool
test c = maybe False c

isValidByr, isValidIyr, isValidEyr, isValidHgt, isValidHcl, isValidEcl, isValidPid :: String -> Bool
isValidByr = isValidYear 1920 2002
isValidIyr = isValidYear 2010 2020
isValidEyr = isValidYear 2020 2030
isValidHgt s = fromMaybe False $ inches <|> cm
  where
    inches = do
      (a:b:'i':'n':[]) <- return s
      v <- readMaybe [a,b]
      return $ v >= 59 && v <= 76
    cm = do
      (a:b:c:'c':'m':[]) <- return s
      v <- readMaybe [a,b,c]
      return $ v >= 150 && v <= 193
isValidHcl s = fromMaybe False $ do
  ('#':rs) <- return s
  return $ length rs == 6 && all (`elem` (['0'..'9'] ++ ['a'..'f'])) rs
isValidEcl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidPid s = length s == 9 && maybe False (const True) (readMaybe s :: Maybe Integer)

isValidYear :: Int -> Int -> String -> Bool
isValidYear from to = maybe False (\y -> y >= from && y <= to) . readMaybe
