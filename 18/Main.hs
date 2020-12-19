{-# LANGUAGE LambdaCase #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  f <- readFile "input/18"
  let exprs = map parseExpr . lines $ f
      result = sum . map eval $ exprs
  print result
  let exprs2 = map parseExpr2 . lines $ f
      result2 = sum . map eval $ exprs2
  print result2

type P = Parsec Void String

data Expr
  = Const Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Ord, Show)

parseExpr :: String -> Expr
parseExpr = either (error . errorBundlePretty) id . runParser pExpr "expr"

parseExpr2 :: String -> Expr
parseExpr2 = either (error . errorBundlePretty) id . runParser pExpr2 "expr"

pExpr :: P Expr
pExpr = makeExprParser pTerm operators

pExpr2 :: P Expr
pExpr2 = makeExprParser pTerm2 operators2

sc :: P ()
sc = L.space space1 empty empty

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: String -> P String
symbol = L.symbol sc

pInteger :: P Expr
pInteger = Const <$> lexeme L.decimal

parens :: P a -> P a
parens = between (symbol "(") (symbol ")")

pTerm :: P Expr
pTerm = choice
  [ parens pExpr
  , pInteger
  ]

pTerm2 :: P Expr
pTerm2 = choice
  [ parens pExpr2
  , pInteger
  ]

operators :: [[Operator P Expr]]
operators =
  [ [ binary "*" Mul
    , binary "+" Add
    ]
  ]

operators2 :: [[Operator P Expr]]
operators2 =
  [ [ binary "+" Add ]
  , [ binary "*" Mul ]
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator P Expr
binary  name f = InfixL  (f <$ symbol name)

eval :: Expr -> Int
eval = \case
  Const n -> n
  Add e1 e2 -> eval e1 + eval e2
  Mul e1 e2 -> eval e1 * eval e2
