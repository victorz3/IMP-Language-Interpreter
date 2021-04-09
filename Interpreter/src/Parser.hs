{- |
Module:      Parser 
Description: This library is for parsing programs in our language (IMP).
Mantainer:   agua@ciencias.unam.mx
-}
module Parser where

import Language
import Text.ParserCombinators.Parsec

-- | 'digitsToInteger' parses a String of digits into an integer.
digitsToInteger :: Parser Integer
digitsToInteger = do
  num <- many1 digit
  return (read num)

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = digitsToInteger
          <?> "a natural integer"
  
-- | 'memory' parses memory locations in the IMP syntax.
memory :: Parser Loc
memory = 
  do char 'x'
     char '['
     loc <- many1 digit
     char ']'
     return (Loc (read loc :: Int))

-- | 'arith' parses an arithmetic expression.
arith :: Parser Arit
arith = aOp
        <|> locToArit <$> memory
        <|> intToArit <$> integer
        <?> "an arithmetic expression"


-- | 'aOp' parses an arithmetic operation, be it '+', '-', or '*'.
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

-- | 'arithComp' parses an arithmetic comparison.
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

-- | 'notBool' parses a boolean negation.
notBool :: Parser BoolExp
notBool = do
  string "not"
  spaces
  b <- boolean
  return (Not b)

-- | 'boolBin' parses a binary operation between booleans ('and' or 'or').
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
  
-- | 'boolean' parses a boolean expression.
boolean :: Parser BoolExp
boolean = T <$ string "true"
          <|> F <$ string "false"
          <|> try (arithComp)
          <|> notBool
          <|> boolBin
          <?> "a boolean expression"

-- | 'assigParser' parses an assignment.
assigParser :: Parser Program
assigParser = do
  l <- memory
  spaces
  string ":="
  spaces
  a <- arith
  return (Assign l a)

-- | 'concatParser' parses a concatenation.
concatParser :: Parser Program
concatParser = do
  char '('
  p1 <- program
  char ';'
  spaces
  p2 <- program
  char ')'
  return (Concat p1 p2)

-- | 'whileParser' parses 'while' expressions.
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

-- | 'ifParser' parses 'if' expressions.
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


-- | 'program' parses a 'Program'.
program :: Parser Program
program = try(concatParser)
          <|> Skip <$ string "skip"
          <|> assigParser
          <|> try(whileParser)
          <|> try(ifParser)
          <?> "a valid program"

-- | 'programwoFirstLine' parses a 'Program' ignoring its first line.
programwoFirstLine :: Parser Program
programwoFirstLine = manyTill anyChar newline *> program

{- | 'numberedProgram' parses a string where the first line represents a
     program's number and the second line beyond contains a 'Program'.
     This function returns a 'Tuple' where the first element is the
     'Program''s number and the second element is the 'Program' itself.
-}
numberedProgram :: Parser (Int, Program)
numberedProgram = do
  number <- many1 digit
  let i = read number
  spaces
  p <- program
  return (i, p)

{- | 'numberedProgramHalt' parses a numbered 'Program' and its halting
     parameter.
     The first line of the 'String' is the 'Program''s number.
     The second line indicates maximum number of steps to execute. 
     Third line and beyond is the 'Program' itself.
     This function returns a tuple where the first element is the
     'Program''s number, the second element is the halting parameter, and
     the third element is the 'Program' itself.
-}
numberedProgramHalt :: Parser (Int, Int, Program)
numberedProgramHalt = do
  n <- many1 digit
  let number = read n
  spaces
  h <- many1 digit
  let halt = read h
  spaces
  p <- program
  return (number, halt, p)
          
 
