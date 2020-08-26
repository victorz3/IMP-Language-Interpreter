--Parser for while programs
--Author: Victor Zamora
import Text.ParserCombinators.Parsec

-- Parses a negative integer.
negative :: Parser String
negative = do
  sign <- char '-'
  number <- many1 digit
  return (sign:number)

--Parses any integer.
integer :: Parser String
integer = negative <|> many1 digit

--References
memory :: Parser String
memory = (string "x[" *> many1 digit) <* char ']'
  
--Arithmetic expressions
arith :: Parser String
arith = integer
        <|> memory
        <|> aOp

--Arithmetic operation
aOp :: Parser String
aOp = do
  char '('
  a1 <- arith
  spaces
  op <- char '+' <|> char '-' <|> char '*'
  spaces
  a2 <- arith
  char ')'
  return (a1 ++ [op] ++ a2)

--Arithmetic comparison
arithComp :: Parser String
arithComp = do
  char '('
  a1 <- arith
  spaces
  op <- char '=' <|> char '<'
  spaces
  a2 <- arith
  char ')'
  return (a1 ++ [op] ++ a2)


--Boolean expression
boolean :: Parser String
boolean = string "true"
          <|> string "false"
          <|> arithComp

 
