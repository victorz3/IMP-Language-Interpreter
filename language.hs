import Numeric.Natural

type State = [(Natural, Integer)]

data Program = Skip
             | Assign Loc Arithmetic
             | Concat Program Program
             | If BoolExp Program Program
             | While BoolExp Program deriving (Show)

data Arithmetic = In Integer
                | Plus Arithmetic Arithmetic
                | Minus Arithmetic Arithmetic
                | Times Arithmetic Arithmetic deriving (Show)

data BoolExp = T
             | F
             | Equals Arithmetic Arithmetic
             | LT Arithmetic Arithmetic
             | Not BoolExp
             | Or BoolExp BoolExp
             | And BoolExp BoolExp deriving (Show)

data Loc = Loc Natural deriving (Show)
  
-- Defining a simple eval function just cause
--eval :: Program -> Integer
