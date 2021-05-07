module Expr (Expr(..), parseExpr, showExprPrec) where

import Prelude hiding (between)
import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array (cons, many, some)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (eof, string, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum, digit, letter)

data Expr
  = Var String
  | Lit Int
  | App Expr Expr
  | Abs String Expr
  | CallCC Expr

parensPrec :: Int -> Int -> String -> String
parensPrec n m s
  | n > m = "(" <> s <> ")"
  | otherwise = s

showExprPrec :: Int -> Expr -> String
showExprPrec _ (Var s) = s

showExprPrec _ (Lit i) = show i

showExprPrec p (App f a) = parensPrec p 1 (showExprPrec 1 f <> " " <> showExprPrec 2 a)

showExprPrec p (Abs v e) = parensPrec p 0 ("λ" <> v <> ". " <> showExprPrec 0 e)

showExprPrec p (CallCC f) = parensPrec p 1 ("call/cc " <> showExprPrec 2 f)

instance showExpr :: Show Expr where
  show e = showExprPrec 0 e

type Parser a
  = ParserT String Identity a

symbol :: String -> Parser Unit
symbol s = void (string s *> whiteSpace)

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* whiteSpace

ident :: Parser String
ident = lexeme (map fromCharArray (cons <$> letter <*> many alphaNum))

literalInt :: Parser Expr
literalInt = do
  digits <- some digit
  case fromString (fromCharArray digits) of
    Nothing -> fail "Expected an integer"
    Just x -> pure (Lit x)

expr :: Parser Expr
expr =
  fix \p ->
    buildExprParser
      [ [ Infix (pure App) AssocLeft, Prefix (symbol "call/cc " $> CallCC) ] ]
      (atom p)
  where
  atom p =
    lexeme
      $ map Var ident
      <|> literalInt
      <|> Abs
      <$> (symbol "\\" *> ident)
      <*> (symbol "." *> p)
      <|> between (symbol "(") (symbol ")") p

parseExpr :: String -> Either String Expr
parseExpr s = case runParser s (whiteSpace *> expr <* eof) of
  Left e -> Left (parseErrorMessage e)
  Right x -> Right x
