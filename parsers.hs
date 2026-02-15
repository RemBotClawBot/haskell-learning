{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonadComprehensions #-}

-- | Parser Combinators in Haskell
-- Building a monadic parser combinator library from scratch,
-- demonstrating functional composition and monad transformers.
module Parsers where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List

-- ============================================================
-- Part 1: Basic Parser Type and Primitive Parsers
-- ============================================================

-- | A parser is a function from input string to possible result
--   plus remaining input.
newtype Parser a = Parser { 
  runParser :: String -> [(a, String)] 
}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    [(f x, rest) | (x, rest) <- runParser p input]

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> [(x, input)]
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = Parser $ \input ->
    [(f x, rest2) | (f, rest1) <- runParser pf input,
                    (x, rest2) <- runParser px rest1]

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const []
  
  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser $ \input ->
    case runParser p input of
      [] -> runParser q input
      results -> results

instance Monad Parser where
  return = pure
  
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \input ->
    [(y, rest2) | (x, rest1) <- runParser p input,
                  (y, rest2) <- runParser (f x) rest1]

-- ============================================================
-- Part 2: Primitive Parsers
-- ============================================================

-- | Parse a single character satisfying a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go [] = []
    go (c:cs)
      | p c = [(c, cs)]
      | otherwise = []

-- | Parse a specific character
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Parse any single character
anyChar :: Parser Char
anyChar = satisfy (const True)

-- | Parse a specific string
string :: String -> Parser String
string [] = pure []
string (c:cs) = do
  char c
  string cs
  pure (c:cs)

-- | Parse a digit
digit :: Parser Char
digit = satisfy isDigit

-- | Parse a letter
letter :: Parser Char
letter = satisfy isLetter

-- | Parse whitespace
whitespace :: Parser Char
whitespace = satisfy isSpace

-- | Parse zero or more whitespace characters
spaces :: Parser String
spaces = many whitespace

-- | Parse one or more whitespace characters
spaces1 :: Parser String
spaces1 = some whitespace

-- ============================================================
-- Part 3: Combinators
-- ============================================================

-- | Parse zero or more occurrences
many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

-- | Parse one or more occurrences
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

-- | Parse optional value
optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Parse separated values
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse one or more separated values
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | Parse surrounded by delimiters
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- | Parse with lookahead
lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \input ->
  case runParser p input of
    [] -> []
    [(x, _)] -> [(x, input)]

-- ============================================================
-- Part 4: Numeric Parsers
-- ============================================================

-- | Parse an integer
integer :: Parser Integer
integer = read <$> some digit

-- | Parse a signed integer
signedInteger :: Parser Integer
signedInteger = do
  sign <- optional (char '-' <|> char '+')
  num <- integer
  pure $ case sign of
    Just '-' -> -num
    _ -> num

-- | Parse a floating point number
float :: Parser Double
float = do
  sign <- optional (char '-' <|> char '+')
  wholePart <- some digit
  decimalPart <- optional (char '.' *> some digit)
  let fullNum = wholePart ++ maybe "" ('.':) decimalPart
  pure $ case sign of
    Just '-' -> -read fullNum
    _ -> read fullNum

-- ============================================================
-- Part 5: JSON Parser Example
-- ============================================================

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- | Parse JSON null
jsonNull :: Parser JsonValue
jsonNull = string "null" $> JsonNull

-- | Parse JSON boolean
jsonBool :: Parser JsonValue
jsonBool = (string "true" $> JsonBool True)
       <|> (string "false" $> JsonBool False)

-- | Parse JSON number
jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> float

-- | Parse JSON string (simplified)
jsonString :: Parser JsonValue
jsonString = do
  char '"'
  content <- many (satisfy (/= '"'))
  char '"'
  pure $ JsonString content

-- | Parse JSON array
jsonArray :: Parser JsonValue
jsonArray = between (char '[') (char ']') $ do
  spaces
  items <- sepBy jsonValue (char ',' <* spaces)
  spaces
  pure $ JsonArray items

-- | Parse JSON object
jsonObject :: Parser JsonValue
jsonObject = between (char '{') (char '}') $ do
  spaces
  pairs <- sepBy keyValuePair (char ',' <* spaces)
  spaces
  pure $ JsonObject pairs
  where
    keyValuePair = do
      key <- between (char '"') (char '"') (many $ satisfy (/= '"'))
      spaces
      char ':'
      spaces
      value <- jsonValue
      pure (key, value)

-- | Parse any JSON value
jsonValue :: Parser JsonValue
jsonValue = spaces *> (
    jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject
  ) <* spaces

-- ============================================================
-- Part 6: Arithmetic Expression Parser
-- ============================================================

data Expr
  = Number Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)

-- | Parse arithmetic expression with precedence
expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = power `chainr1` powOp

power :: Parser Expr
power = between (char '(') (char ')') expr
    <|> Number <$> float

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (char '+' $> Add) <|> (char '-' $> Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = (char '*' $> Mul) <|> (char '/' $> Div)

powOp :: Parser (Expr -> Expr -> Expr)
powOp = char '^' $> Pow

-- Chain combinators
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = foldl (flip ($)) <$> p <*> many (do
    f <- op
    y <- p
    pure (flip f y))

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do
  x <- p
  optional (do
    f <- op
    y <- chainr1 p op
    pure (f x y)) <|> pure x

-- ============================================================
-- Part 7: Practical Examples
-- ============================================================

-- | Parse CSV line
csvLine :: Parser [String]
csvLine = sepBy cell (char ',')
  where
    cell = many (satisfy (/= ','))

-- | Parse key-value pairs (e.g., "key=value")
keyValuePairs :: Parser [(String, String)]
keyValuePairs = sepBy keyValuePair (char ';')
  where
    keyValuePair = do
      key <- many1 (satisfy (/= '='))
      char '='
      value <- many (satisfy (/= ';'))
      pure (key, value)

-- | Parse a simple configuration file
configParser :: Parser [(String, String)]
configParser = many configLine
  where
    configLine = do
      spaces
      key <- some (satisfy isAlphaNum)
      spaces
      char '='
      spaces
      value <- many (satisfy (not . isSpace))
      spaces
      optional (char '\n')
      pure (key, value)

-- ============================================================
-- Part 8: Running Examples
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Parser Combinator Examples ===\n"
  
  -- Test basic parsers
  putStrLn "1. Basic Parsers:"
  print $ runParser digit "123"
  print $ runParser (string "hello") "hello world"
  print $ runParser integer "42 apples"
  
  -- Test JSON parser
  putStrLn "\n2. JSON Parsing:"
  let jsonInput = "{\"name\": \"Alice\", \"age\": 30, \"active\": true}"
  print $ runParser jsonValue jsonInput
  
  -- Test arithmetic parser
  putStrLn "\n3. Arithmetic Expression Parsing:"
  let expr1 = "3 + 4 * 2"
      expr2 = "(3 + 4) * 2"
      expr3 = "2 ^ 3 ^ 2"
  
  putStrLn $ "Expression: " ++ expr1
  print $ runParser expr expr1
  
  putStrLn $ "\nExpression: " ++ expr2
  print $ runParser expr expr2
  
  putStrLn $ "\nExpression: " ++ expr3
  print $ runParser expr expr3
  
  -- Test CSV parsing
  putStrLn "\n4. CSV Line Parsing:"
  let csv = "apple,banana,cherry,dates"
  print $ runParser csvLine csv
  
  -- Test config parsing
  putStrLn "\n5. Configuration Parsing:"
  let config = "debug=true\nlevel=high\ntimeout=30"
  print $ runParser configParser config
  
  putStrLn "\n=== Parser Examples Complete ==="