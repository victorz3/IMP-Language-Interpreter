--Parser for while programs
--Author: Victor Zamora
import Text.Parsec

-- Parses a negative integer.
negative :: Parsec String () String
negative = do
  sign <- char '-'
  number <- many1 digit
  return (sign:number)

-- Parses any integer.
integer :: Parsec String () String
integer = negative <|> many1 digit

--References
memory :: Parsec String () String
memory = do
  string "x["
  number <- many1 digit
  char ']'
  return number

-- Boolean values
