module Substitution where
    import Type
    import Pretty

    newtype Subst = Subst [(VarIndex, Term)]

    instance (Show Subst) where
       show = pretty

    instance (Pretty Subst) where
        prettyWithVars v (Subst []) = "{}"
        prettyWithVars v (Subst (head:tail)) = "{"
                                        ++ substTupToString v head
                                        ++ [ x |tuple<-tail,x <- ","++ substTupToString v tuple ]
                                        ++ "}"

    substTupToString::[(VarIndex,String)]->(VarIndex, Term)->String
    substTupToString v (index, term) = prettyWithVars v (Var index)++" -> "++ prettyWithVars v term

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

    {-
            Inserts a Substitution Tuple into a Substitution Tuple Set
                if no Substitution existst fo the given Variable Index
    -}
    insertSubst::Subst->(VarIndex, Term)->Subst
    insertSubst (Subst [])                      a                              = Subst [a]
    insertSubst (Subst (head@(index, _):xs)) ins@(aIndex, _) | index == aIndex = Subst  (head:xs)
                                                             | otherwise       = let
                                                                                    (Subst tail) = insertSubst (Subst xs) ins
                                                                                 in
                                                                                    Subst  (head:tail)

    --todo fix compose
    compose::Subst->Subst->Subst
    compose (Subst []) a          = a
    compose a          (Subst []) = a
    compose (Subst a)  (Subst b)  = Subst (b++a)

