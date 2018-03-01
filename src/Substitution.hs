module Substitution where
    import Data.Bifunctor
    import Lib

    empty :: Subst
    empty = Subst []

    single :: VarIndex -> Term -> Subst
    single index term = Subst [(index, term)]

    apply :: Subst -> Term -> Term
    apply (Subst []) orig = orig
    apply (Subst ((index, term):rest)) orig@(Var vIndex) | index == vIndex  = term
                                                         | otherwise        = apply (Subst rest) orig
    apply subst                   (Comb s term)  = Comb s (map (apply subst) term)

    {-
        apply for a list of Terms resulting in a new goal
    -}
    (->>) :: Subst -> [Term] -> Goal
    (->>) subst terms = Goal (map (apply subst) terms)

    compose :: Subst -> Subst -> Subst
    compose (Subst []) a          = a
    compose a          (Subst []) = a
    compose (Subst a)  (Subst b)  = Subst $ map (second (apply $ Subst a)) b ++ [add | add@(index, _) <- a,index `notElem` [i | (i,_) <- b]]
