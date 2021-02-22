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
 
-- Returns the size of a program in AST length.
lenP :: Program -> Int
lenP Skip = 1
lenP (Assign l a) = 1 + (lenL l) + (lenA a)
lenP (Concat p1 p2) = 1 + (lenP p1) + (lenP p2)
lenP (If b p1 p2) = 1 + (lenB b) + (lenP p1) + (lenP p2)
lenP (While b p) = 1 + (lenB b) + (lenP p)
lenP NoHalt = 1

-- Returns the size of a location in AST length.
lenL :: Loc -> Int
lenL (Loc _) = 2

-- Return the size of an arithmetic expression in AST length.
lenA :: Arit -> Int
lenA (In _) = 2
lenA (Mem l) = 1 + (lenL l)
lenA (Plus a1 a2) = 1 + (lenA a1) + (lenA a2)
lenA (Minus a1 a2) = 1 + (lenA a1) + (lenA a2)
lenA (Times a1 a2) = 1 + (lenA a1) + (lenA a2)

-- Returns the size of a boolean expression in AST length.
lenB :: BoolExp -> Int
lenB T = 1
lenB F = 1
lenB (Equals a1 a2) = 1 + (lenA a1) + (lenA a2)
lenB (Lessthan a1 a2) = 1 + (lenA a1) + (lenA a2)
lenB (Not b) = 1 + (lenB b)
lenB (Or b1 b2) = 1 + (lenB b1) + (lenB b2)
lenB (And b1 b2) = 1 + (lenB b1) + (lenB b2)

