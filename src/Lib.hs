{-|
    This Module provides Classes, Instances and Types
-}
module Lib(
    module Lib,module Type,module Pretty
    ) where
    import Type
    import Pretty

-- = Types

    -- == REPL Types

    -- | This Type is used for the current internal state of the REPL
    type State = (Strategy,Prog)
    -- | This Type if for Actions that can be evoked from the main prompt of the REPL
    type Action = State -> String -> IO ()

    -- == SLDTree Types

    -- | Just the signature of the sld Function in the SLD module
    type SLD = (Strategy->Prog->Goal->SLDTree)

    {-|
       This type is used for performing substitutions.
       The SLD module provides a function for converting Prolog Rules into this.
    -}
    type BuildInRule = SLD -> Strategy -> Prog -> Goal -> Maybe (Subst, Goal)

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

    -- | This type represents a Strategy used to generate the result list from an SLDTree
    type Strategy = SLDTree -> [Subst]



-- = Instances
    instance (Pretty Subst) where
        prettyWithVars v (Subst [])          = "{}"
        prettyWithVars v (Subst (head:tail)) = "{"
                                        ++ substTupToString v head
                                        ++ [ x|tuple<-tail,x <- ","++ substTupToString v tuple]
                                        ++ "}"
          where
            -- |prettyWithVars for Tuples that are part of a Substitution
            substTupToString :: [(VarIndex, String)] -> (VarIndex, Term) -> String
            substTupToString v (index, term)
                = prettyWithVars v (Var index) ++ " -> " ++ prettyWithVars v term

    instance Pretty SLDTree where
        prettyWithVars _  Success          = "Success"
        prettyWithVars v (SLDTree stuff)   = "SLDTree [" ++ prettyWithVars v stuff ++ "]"

    -- |To not edit Type we just implemented it ourselves
    instance (Eq Term) where
        (==) (Var i) (Var i2) = i == i2
        (==) (Comb s r) (Comb s2 r2) | s == s2 = all (uncurry (==)) (zip r r2)
        (==) _ _ = False

-- == Useful Stuff

    -- | returns True iff a Variable with the given Index is part of the Term
    isIn :: VarIndex -> Term -> Bool
    isIn a (Var b) = a == b
    isIn a (Comb b tb) = any (isIn a) tb

    -- | returns the List of Variable Indices used by the Term)
    varsInUse :: Term -> [VarIndex]
    varsInUse (Var v)        = [v]
    varsInUse (Comb _ terms) = [ x |term<-terms, x<-varsInUse term]
