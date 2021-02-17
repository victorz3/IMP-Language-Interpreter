-- Abstract syntax for while programs
-- Author: Victor Zamora
module Language where

-- A program
data Program = Skip
             | Assign Loc Arit
             | Concat Program Program
             | If BoolExp Program Program
             | While BoolExp Program
             -- Special, non-halting program
             | NoHalt deriving (Show, Eq)

-- Arithmetic expression
data Arit = In Integer
          | Mem Loc
          | Plus Arit Arit
          | Minus Arit Arit
          | Times Arit Arit deriving (Show, Eq)

-- Boolean Expression
data BoolExp = T
             | F
             | Equals Arit Arit
             | Lessthan Arit Arit
             | Not BoolExp
             | Or BoolExp BoolExp
             | And BoolExp BoolExp deriving (Show, Eq)

--Memory location
data Loc = Loc Int deriving (Show, Eq)
 
