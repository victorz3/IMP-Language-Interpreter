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
integer = negative
          <|> digitsToInteger
          <?> "an integer"
  
-- References
memory :: Parser Loc
memory = 
  do char 'x'
     char '['
     loc <- many1 digit
     char ']'
     return (Loc (read loc :: Natural))

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
          
-- For testing.
main =
    do c <- getContents
       case parse program "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> (print r)

          
          
          
 
