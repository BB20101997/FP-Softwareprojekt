{-|
    This Module provides Classes, Instances and Types
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib  ( module Lib
            , module Type
            , module Pretty
            ) where

    import Type
    import Pretty

-- = Types

    -- == REPL Types

    -- | This Type is used for the current internal state of the REPL
    data State
     = State
        { strategy      :: Strategy
        , program       :: Prog
        , fileParser    :: FilePath -> IO (Either String Prog)
        , goalParser    :: String -> Either String (Goal, [(VarIndex, String)])
        }

    {-|
        This is the Type of the REPL's main prompt actions
    -}
    type Action = State -> String -> IO ()

    -- == SLDTree Types

    {-|
        The Parameter for the internal state of the sld function
    -}
    data SLDParameter = SLDParameter
                        { usedBuildIn  :: [BuildInRule]
                        , usedStrategy :: Strategy
                        , usedVars     :: [VarIndex]
                        , usedProgram  :: Prog
                        }

    {-|
        The Type for Build in Prolog Predicates
    -}
    type BuildInRule = (String, RuleApplicator)

    {-|
        This is the Type of a function
        that applies the substitution for one specific rule
    -}
    type RuleApplicator = SLDParameter -> Goal -> Maybe (Subst, Goal)

    {-|
        This is the type of a SLDTree,
        the Success constructor is used to mark the end of a Successful Branch
    -}
    data SLDTree = SLDTree [(Subst, SLDTree)] | Success
        deriving() -- I don't know why this is necessary

    -- == Substitution Types

    -- | This type represents a List of Variable to Term Substitutions
    newtype Subst = Subst [(VarIndex, Term)]

    -- == Strategy Types

    {-|
        The Type for SLDTree solution search Strategies
    -}
    type Strategy = SLDTree -> [Subst]

-- = Instances

    -- | Pretty Substitutions
    instance (Pretty Subst) where
        prettyWithVars _ (Subst [])
            = "{}"
        prettyWithVars v (Subst (x:xs))
            =   '{':substTupToString x
                ++ [t | tuple<-xs, t <- ", " ++ substTupToString tuple]
                ++ "}"
          where
            -- |prettyWithVars for Tuples that are part of a Substitution
            substTupToString :: (VarIndex, Term) -> String
            substTupToString (index, term)
                = prettyWithVars v (Var index)
                    ++ " -> "
                    ++ prettyWithVars v term

    -- | Pretty SLDTree
    instance Pretty SLDTree where
        prettyWithVars _  Success
            = "Success"
        prettyWithVars v (SLDTree stuff)
            = "SLDTree [" ++ prettyWithVars v stuff ++ "]"

    -- |To not edit Type we just implemented it ourselves
    instance  (Eq Term) where
        (==) (Var i)    (Var i2)     = i == i2
        (==) (Comb s r) (Comb s2 r2) | s == s2
                                     , length r == length r2
                                     = all (uncurry (==)) (zip r r2)
        (==) _          _            = False

-- == Useful Stuff

    -- | returns True iff a Variable with the given Index is part of the Term
    isIn :: VarIndex -> Term -> Bool
    isIn a (Var b)     = a == b
    isIn a (Comb _ ts) = any (isIn a) ts

    -- | returns the List of Variable Indices used by the Term)
    varsInUse :: Term -> [VarIndex]
    varsInUse (Var v)        = [v]
    varsInUse (Comb _ terms) = [x | term <- terms, x <- varsInUse term]

    -- | returns the List of Variable Indices used by the Goal
    varsInGoal :: Goal -> [VarIndex]
    varsInGoal (Goal terms) = concatMap varsInUse terms


