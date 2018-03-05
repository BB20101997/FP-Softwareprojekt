{-|
    This module implements Substitutions
-}
module Substitution where
    import qualified Data.Bifunctor as Bi

    import qualified Lib
    import Lib(Subst(..), VarIndex, Term(..), Goal(..), Rule(..))

    -- |An empty Substitution
    empty :: Subst
    empty = Subst []

    -- |Creates a Substitution for a VarIndex and a Term)
    single :: VarIndex -> Term -> Subst
    single index term = Subst [(index, term)]

    -- |Applies a Substitution to a Term
    apply :: Subst -> Term -> Term
    apply (Subst []) orig = orig
    apply (Subst ((index, term):rest)) orig@(Var vIndex)
        | index == vIndex      = term
        | otherwise            = apply (Subst rest) orig
    apply subst (Comb s term)  = Comb s (map (apply subst) term)

    -- |Applies a Substitution on a list of Terms wrapping the result in a goal
    (->>) :: Subst -> [Term] -> Goal
    (->>) subst terms = Goal (map (apply subst) terms)

    -- |Composes two Substitutions into one
    compose :: Subst -> Subst -> Subst
    compose (Subst []) a
        =       a
    compose a          (Subst [])
        =       a
    compose (Subst a)  (Subst b)
        =   let
                a' = map (Bi.second (apply $ Subst a)) b
                b' = [elm | elm@(ai, _) <- a, ai `notElem` [bi | (bi, _) <- b]]
            in
                Subst $ a' ++ b'

    {-|
        Produces a functionally identical Rule to the input Rule
        The resulting rule will have all rules present in the Goal
        replaced by new ones
    -}
    (><) :: Rule -> [VarIndex] -> Rule
    (><) (pat :- cond) v
        = let
            -- list of used Variables in the Pattern
            usedR   = concatMap Lib.varsInUse (pat:cond)
            -- list of unused Variables
            notUsed = [ x | x <- [0, 1 .. ], x `notElem` v, x `notElem` usedR]
            -- already used and used in the rule
            vs      = [i | i <- v, i `elem` usedR]
            -- create a Substitution for creating then new Rule
            subst   = Subst [(i, Var (notUsed !! i)) | i <- vs]
            -- creating pattern and condition for new Rule
            pat'    = apply subst pat
          in
            pat' :- map (apply subst) cond
