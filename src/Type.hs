module Type
  ( VarIndex, Term(..), Rule(..), Prog(..), Goal(..)
  ) where

-- Alias type for variables
type VarIndex = Int

-- Data type for terms
data Term = Var VarIndex | Comb String [Term]
  deriving Show

-- Data type for program rules
data Rule = Term :- [Term]
  deriving Show

-- Data type for programs
data Prog = Prog [Rule]
  deriving Show

-- Data type for goals
data Goal = Goal [Term]
  deriving Show

