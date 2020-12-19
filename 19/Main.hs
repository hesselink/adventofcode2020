{-# LANGUAGE LambdaCase #-}
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Map (Map)
import Data.Maybe (fromMaybe, fromJust, listToMaybe, isJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import qualified Data.Map as Map
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.ParserCombinators.ReadP as R

main :: IO ()
main = do
  f <- readFile "input/19"
  let input = parseInput f
      rs = rules input
      r0 = fromMaybe (error "rule 0 not found") $ Map.lookup 0 rs
      p0 = toParser rs r0
      result = length $ filter (isMatch p0) (messages input)
  print result
  let newRs = Map.insert 8 new8 (Map.insert 11 new11 rs)
      new8 = parseRule "42 | 42 8"
      new11 = parseRule "42 31 | 42 11 31"
      p0_2 = toParser newRs r0
      result2 = length $ filter (isMatch p0_2) (messages input)
  print result2

data Input = Input
  { rules :: Map Int Rule
  , messages :: [String]
  } deriving (Show, Eq)

data Rule
  = Token Char
  | Ref Int
  | Seq Rule Rule
  | Choice Rule Rule
  deriving (Show, Eq)

data NRule
  = Prims [Prim]
  | Seqs [NRule]
  | Choices [NRule]
  deriving (Show, Eq, Ord)

data Prim = PToken Char | PRef Int
  deriving (Show, Eq, Ord)

isMatch :: ReadP a -> String -> Bool
isMatch p = isJust . runR p

runP :: P a -> String -> a
runP p = either (error . errorBundlePretty) id . runParser p "input"

parseInput :: String -> Input
parseInput = runP pInput

parseRule :: String -> Rule
parseRule = runP pRuleExpr

type P = Parsec Void String

pInput :: P Input
pInput = Input <$> pRules <* newline <*> pMessages <?> "input"

pRules :: P (Map Int Rule)
pRules = Map.fromList <$> pRule `endBy1` newline <?> "rules"

pRule :: P (Int, Rule)
pRule = (,) <$> pInteger <* string ": " <*> pRuleExpr <?> "rule"

pRuleExpr :: P Rule
pRuleExpr = makeExprParser pPrimRule [[InfixR (Seq <$ pure ())], [InfixR (Choice <$ symbol "|")]]

pToken :: P Rule
pToken = Token <$> between (char '"') (char '"') L.charLiteral <?> "token"

pRef :: P Rule
pRef = Ref <$> pInteger <?> "ref"

pPrimRule :: P Rule
pPrimRule = pToken <|> pRef <?> "prim"

pMessages :: P [String]
pMessages = takeWhileP (Just "message char") (/= '\n') `endBy` newline

pInteger :: P Int
pInteger = lexeme L.decimal <?> "integer"

sc :: P ()
sc = L.space (void $ takeWhile1P (Just "space") (== ' ')) empty empty

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: String -> P String
symbol = L.symbol sc

runR :: ReadP a -> String -> Maybe a
runR r s = fmap fst . listToMaybe . filter (null . snd) . readP_to_S r $ s

getSingleton :: [a] -> Maybe a
getSingleton [x] = Just x
getSingleton _ = Nothing

toParser :: Map Int Rule -> Rule -> ReadP ()
toParser rs = \case
  Token t -> () <$ R.char t
  Ref i -> toParser rs (fromJust $ Map.lookup i rs)
  Seq r1 r2 ->  toParser rs r1 <* toParser rs r2
  Choice r1 r2 -> toParser rs r1 <|> toParser rs r2
