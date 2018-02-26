module Substitution where
    import Type
    import Pretty
    type Subst = [(VarIndex, Term)]

    empty::Subst
    empty = []

    single::VarIndex -> Term -> Subst
    single index term = [(index, term)]

    apply::Subst -> Term -> Term
    apply [(index, term)] orig@(Var vIndex) | index == vIndex  = term
                                            | otherwise        = orig
    apply subst orig@(Var _)   = foldl (\var term -> apply [term] var) orig subst
    apply subst (Comb s terms) = Comb s (map (apply subst) terms)

    insertSubst::Subst->(VarIndex, Term)->Subst
    insertSubst []                 a                                      = [a]
    insertSubst ((index, term):xs) (aIndex, aTerm) | index == aIndex = (index, term):xs
                                                   | otherwise       = (index, term):(insertSubst xs (aIndex, aTerm))

    compose::Subst->Subst->Subst
    compose [] a = a
    compose a [] = a
    compose a b = foldl insertSubst b a

    --TODO implement pretty instance
    --instance (Pretty Subst) where

