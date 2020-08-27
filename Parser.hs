--Parser for while programs
--Author: Victor Zamora
import Text.ParserCombinators.Parsec
import Language
import Numeric.Natural

-- Parses a negative integer.
negative :: Parser Integer
negative = do
  sign <- char '-'
  number <- many1 digit
  return (read (sign:number))

-- Parses digits into an integer.
digitsToInteger :: Parser Integer
digitsToInteger = do
  num <- many1 digit
  return (read num)

--Parses any integer.
integer :: Parser Integer
integer = negative <|> digitsToInteger

-- References
memory :: Parser Loc
memory = do
  char 'x'
  char '['
  loc <- many1 digit
  char ']'
  return (Loc (read loc :: Natural))

--Arithmetic expressions
--arith :: Parser String
--arith = do
 -- exp <- integer <|> memory
  --case exp 
        -- <|> aOp


-- --Arithmetic operation
-- aOp :: Parser String
-- aOp = do
--   char '('
--   a1 <- arith
--   spaces
--   op <- char '+' <|> char '-' <|> char '*'
--   spaces
--   a2 <- arith
--   char ')'
--   return (a1 ++ [op] ++ a2)


-- --Arithmetic comparison
-- arithComp :: Parser String
-- arithComp = do
--   char '('
--   a1 <- arith
--   spaces
--   op <- char '=' <|> char '<'
--   spaces
--   a2 <- arith
--   char ')'
--   return (a1 ++ [op] ++ a2)


-- --Boolean expression
-- boolean :: Parser String
-- boolean = string "true"
--           <|> string "false"
--           <|> arithComp

 
