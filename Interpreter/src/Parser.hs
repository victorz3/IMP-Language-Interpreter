--Parser for while programs
--Author: Victor Zamora
module Parser where

import Text.ParserCombinators.Parsec
import Language

-- Parses a natural number.
digitsToInteger :: Parser Integer
digitsToInteger = do
  num <- many1 digit
  return (read num)

-- Parser for natural integers.
integer :: Parser Integer
integer = digitsToInteger
          <?> "a natural integer"
  
-- References
memory :: Parser Loc
memory = 
  do char 'x'
     char '['
     loc <- many1 digit
     char ']'
     return (Loc (read loc :: Int))

--Converts location to arithmetic expression
locToArit :: Loc -> Arit
locToArit x = Mem x

--Converts integer to arithmetic expression
intToArit :: Integer -> Arit
intToArit i = In i


--Arithmetic expressions
arith :: Parser Arit
arith = aOp
        <|> locToArit <$> memory
        <|> intToArit <$> integer
        <?> "an arithmetic expression"


--Arithmetic operation
aOp :: Parser Arit
aOp = do
  char '('
  a1 <- arith
  spaces
  op <- char '+' <|> char '-' <|> char '*'
  spaces
  a2 <- arith
  char ')'
  case op of
    '+' -> return (Plus a1 a2) 
    '-' -> return (Minus a1 a2)
    '*' -> return (Times a1 a2)

--Arithmetic comparison
arithComp :: Parser BoolExp
arithComp = do
  char '('
  a1 <- arith
  spaces
  op <- char '=' <|> char '<'
  spaces
  a2 <- arith
  char ')'
  case op of
    '=' -> return (Equals a1 a2)
    '<' -> return (Lessthan a1 a2)

-- Parses a boolean negation.
notBool :: Parser BoolExp
notBool = do
  string "not"
  spaces
  b <- boolean
  return (Not b)

-- Parses a boolean binary operation.
boolBin :: Parser BoolExp
boolBin = do
  char '('
  b1 <- boolean
  spaces
  op <- string "or" <|> string "and"
  spaces
  b2 <- boolean
  char ')'
  case op of
    "or" -> return (Or b1 b2)
    "and" -> return (And b1 b2)
  
--Boolean expression
boolean :: Parser BoolExp
boolean = T <$ string "true"
          <|> F <$ string "false"
          <|> try (arithComp)
          <|> notBool
          <|> boolBin
          <?> "a boolean expression"

-- Parses an assignment
assigParser :: Parser Program
assigParser = do
  l <- memory
  spaces
  string ":="
  spaces
  a <- arith
  return (Assign l a)

-- Parses a concatenation.
concatParser :: Parser Program
concatParser = do
  char '('
  p1 <- program
  char ';'
  spaces
  p2 <- program
  char ')'
  return (Concat p1 p2)

-- While expression parser
whileParser :: Parser Program
whileParser = do
  string "(while"
  spaces
  b <- boolean
  spaces
  string "do"
  spaces
  p <- program
  char ')'
  return (While b p)

-- If expression parser
ifParser :: Parser Program
ifParser = do
  string "(if"
  spaces
  b <- boolean
  spaces
  string "then"
  spaces
  p1 <- program
  spaces
  string "else"
  spaces
  p2 <- program
  char ')'
  return (If b p1 p2)
  
--Parses a program
program :: Parser Program
program = try(concatParser)
          <|> Skip <$ string "skip"
          <|> assigParser
          <|> try(whileParser)
          <|> try(ifParser)
          <?> "a valid program"

-- Parses program without using the first line
parsewoFirstLine :: Parser Program
parsewoFirstLine = manyTill anyChar newline *> program

{- Parses a numbered program. The first line of the String is the number and the
 - rest is the program. -}
numberedProgram :: Parser (Int, Program)
numberedProgram = do
  number <- many1 digit
  let i = read number
  spaces
  p <- program
  return (i, p)

{- Parses a numbered program and its halting parameter. The first line of the String
 - is the program's number. The second line indicates number of steps to run. Third
 - line and beyond is the program. -}
numberedProgramWithHalt :: Parser (Int, Int, Program)
numberedProgramWithHalt = do
  n <- many1 digit
  let number = read n
  spaces
  h <- many1 digit
  let halt = read h
  spaces
  p <- program
  return (number, halt, p)
          
 
