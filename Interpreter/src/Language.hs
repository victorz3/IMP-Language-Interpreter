{- |
Module:      Language 
Description: Module 'Language' contains the abstract syntax of our language
             (IMP).
Mantainer:   agua@ciencias.unam.mx
-}
module Language where

-- | Abstract syntax for programs.
data Program
  = Skip                       -- | skip program.
  | Assign Loc Arit            -- | Assignments.
  | Concat Program Program     -- | Concatenation.
  | If BoolExp Program Program -- | if expressions.
  | While BoolExp Program      -- | while expressions.
  | NoHalt                     -- | Special constructor for programs that
                               --   we know won't halt.
  deriving (Show, Eq)

-- | Abstract syntax for arithmetic expressions.
data Arit
  = In Integer        -- | Integers.
  | Mem Loc           -- | Memory locations.
  | Plus Arit Arit    -- | Sum of two arithmetic expressions.
  | Minus Arit Arit   -- | Subtraction of two arithmetic expressions.
  | Times Arit Arit   -- | Multiplication of two arithmetic expressions.
  deriving (Show, Eq)

-- | Abstract syntax for boolean expressions.
data BoolExp
  = T                   -- | true
  | F                   -- | false
  | Equals Arit Arit    -- | Equality of two arithmetic expressions.
  | Lessthan Arit Arit  -- | Less than for arithmetic expressions.
  | Not BoolExp         -- | Negation of a boolean expression.
  | Or BoolExp BoolExp  -- | Or of two boolean expressions.
  | And BoolExp BoolExp -- | And of two boolean expressions.
  deriving (Show, Eq)

-- | Abstract syntax for memory locations.
data Loc
  = Loc Int -- | A location corresponding to the ith register, where i is
            --   the 'Int' parameter.
  deriving (Show, Eq)
 
-- Returns the length of a program in AST length.
lenP :: Program -> Int
lenP Skip = 1
lenP (Assign l a) = 1 + (lenL l) + (lenA a)
lenP (Concat p1 p2) = 1 + (lenP p1) + (lenP p2)
lenP (If b p1 p2) = 1 + (lenB b) + (lenP p1) + (lenP p2)
lenP (While b p) = 1 + (lenB b) + (lenP p)
lenP NoHalt = 1

-- Returns the length of a location in AST length.
lenL :: Loc -> Int
lenL (Loc n) = 1 + (lenInt n)

-- Return the size of an arithmetic expression in AST length.
lenA :: Arit -> Int
lenA (In n) = lenInt n
lenA (Mem l) = lenL l
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

-- Returns the number of bits necessary to represent an integer.
lenInt :: Integral a => a -> Int
lenInt 0 = 1
lenInt 1 = 1
lenInt n = 1 + (lenInt (div n 2))

-- | 'locToArit' casts a 'Loc' into an 'Arit'.
locToArit :: Loc -> Arit
locToArit x = Mem x
