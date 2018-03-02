module Substitution where
    import Data.Bifunctor

    import Lib

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
                a' = map (second (apply $ Subst a)) b
                b' = [elm | elm@(ai, _) <- a,ai `notElem` [bi | (bi,_) <- b]]
            in
                Subst $ a'++b'
