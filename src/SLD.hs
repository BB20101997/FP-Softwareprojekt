module SLD where
    import Type
    import Substitution
    import Unifikation

    data SLDTree = SLDTree [(Subst,SLDTree)]
        deriving Show

    sld::Prog->Goal->SLDTree
    sld programm@(Prog rules) (Goal terms) = SLDTree [ tp | Just tp <-[ substitute programm rule term
                                                                      | rule <- rules, term<-terms ] ]
        where
            --neue Variablen für Regeln verwenden
            substitute::Prog->Rule->Term ->Maybe (Subst,SLDTree)
            substitute prog (pat :- cond) term = let
                                                    unifier = unify pat term
                                                 in
                                                    case unifier of
                                                        Nothing -> Nothing
                                                        Just subst -> Just (subst,sld prog (Goal (map (apply subst) cond)))




