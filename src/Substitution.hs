module Substitution where
    import Type
    import Pretty

    data Subst = Subst [(VarIndex, Term)]

    instance (Show Subst) where
        show = pretty

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
            Inserts a Substitution Tuple into a Substitution Tuple Set
                if no Substitution existst fo the given Variable Index
    -}
    insertSubst::Subst->(VarIndex, Term)->Subst
    insertSubst (Subst [])                      a                              = Subst [a]
    insertSubst (Subst (head@(index, _):xs)) ins@(aIndex, _) | index == aIndex = (Subst  (head:xs))
                                                             | otherwise       = let
                                                                                    (Subst tail) = (insertSubst (Subst xs) ins)
                                                                                 in
                                                                                    (Subst  (head:tail))

    compose::Subst->Subst->Subst
    compose (Subst []) a          = a
    compose a          (Subst []) = a
    compose (Subst a)  b          = foldl insertSubst b a

    substTupToString::(VarIndex, Term)->String
    substTupToString (index, term) = (pretty (Var index))++" -> "++(pretty (term))

    instance (Pretty Subst) where
        pretty (Subst []) = "{}"
        pretty (Subst (head:tail)) = "{"
                                        ++ (substTupToString head)
                                        ++ [ x |tupel<-tail,x <- ","++ (substTupToString tupel) ]
                                        ++ "}"
