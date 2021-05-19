module Expr (Expr(..), parseExpr, showExprPrec) where

import Prelude hiding (between)

import Control.Alternative (guard, (<|>))
import Control.Lazy (fix)
import Data.Array (cons, many, notElem, some)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust, optional)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (between, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (eof, string, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum, digit, letter)

data Expr
  = Var String
  | Lit Int
  | LitB Boolean
  | If Expr Expr Expr
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

showExprPrec _ (LitB b) = show b

showExprPrec _ (If c t f) = "if " <> showExprPrec 0 c <> " then " <> showExprPrec 0 t <> " else " <> showExprPrec 0 f

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
ident = lexeme $ do
  x <- map fromCharArray (cons <$> letter <*> many alphaNum)
  guard (x `notElem` ["if", "then", "else", "true", "false"])
  pure x

literalInt :: Parser Expr
literalInt = do
  sign <- optional (symbol "-")
  digits <- some digit
  case fromString (fromCharArray digits) of
    Nothing -> fail "Expected an integer"
    Just x -> pure (Lit $ if isJust sign then -x else x)

literalBool :: Parser Expr
literalBool = (LitB true <$ symbol "true") <|> (LitB false <$ symbol "false")

expr :: Parser Expr
expr =
  fix \p ->
    buildExprParser
      [ [ Infix (pure App) AssocLeft, Prefix (symbol "call/cc" $> CallCC) ] ]
      (atom p)
  where
  atom p =
    lexeme
      $ do
          symbol "if"
          c <- p
          symbol "then"
          t <- p
          symbol "else"
          f <- p
          pure (If c t f)
      <|> literalInt
      <|> literalBool
      <|> Abs
      <$> (symbol "\\" *> ident)
      <*> (symbol "." *> p)
      <|> try (map Var ident)
      <|> between (symbol "(") (symbol ")") p

parseExpr :: String -> Either String Expr
parseExpr s = case runParser s (whiteSpace *> expr <* eof) of
  Left e -> Left (parseErrorMessage e)
  Right x -> Right x
