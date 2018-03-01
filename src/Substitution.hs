module Substitution where
    import Lib

    empty::Subst
    empty = Subst []

    single::VarIndex -> Term -> Subst
    single index term = Subst [(index, term)]

    apply::Subst -> Term -> Term
    apply (Subst [(index, term)]) orig@(Var vIndex) | index == vIndex  = term
                                                    | otherwise        = orig
    apply (Subst subst)           orig@(Var _)   = foldl (\var term -> apply (Subst [term]) var) orig subst
    apply subst                   (Comb s term)  = Comb s (map (apply subst) term)

    {-
        apply for a list of Terms resulting in a new goal
    -}
    (->>)::Subst->[Term]->Goal
    (->>) subst terms = Goal (map (apply subst) terms)

    compose::Subst->Subst->Subst
    compose (Subst []) a          = a
    compose a          (Subst []) = a
    compose (Subst a)  (Subst b)  = Subst (b++a)
        where
            --The boolean determines if the substitution should be inserted at the end
            combine::Bool->[(VarIndex,Term)]->(VarIndex,Term)->[(VarIndex,Term)]
            combine True  []                  ins            = [ins]
            combine False []                  ins            = []
            combine end   ((index,term):tail) ins@(vIndex,_) = (index, apply (Subst [ins]) term) : combine (end&&(index/=vIndex))  tail ins
