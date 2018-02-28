module SLD where
    import Data.Bifunctor
    import Data.Char
    import Data.Maybe
    import Lib
    import Substitution
    import Unifikation

    predefinedRulesMap::[(String,BuildInRule)]
    predefinedRulesMap = [("call",callSubstitution),("is",evalSubstitution),("not",notSubstitution),("\\+",notSubstitution)]

    predefinedRules::[Rule]
    predefinedRules = [Comb x [] :- [] | (x,_)<-predefinedRulesMap]


    {-
        For a given Program and Goal produces the corresponding SLDTree based on FIRST Selection-strategy
    -}
    sld::Strategy->Prog->Goal->SLDTree
    sld strategy program@(Prog rules) goal = SLDTree $ catMaybes [substitute rule strategy program goal | rule <- predefinedRules++rules]
        where
            substitute::Rule->BuildInRule
            substitute  _              _        _   (Goal [])                                        = Just (empty,Success)
            substitute (Comb a _ :- _) strategy prog goal | Just func <- lookup a predefinedRulesMap = func strategy prog goal
            substitute  rule           strategy prog goal                                            = baseSubstitution rule strategy prog goal

    baseSubstitution::Rule->BuildInRule
    baseSubstitution rule strategy prog goal@(Goal (term:rest)) =   let
                                                                        (pat :- cond)      = rule >< goal
                                                                        unifier            = unify term pat
                                                                    in case unifier of
                                                                        Nothing     -> Nothing
                                                                        Just subst  ->  let
                                                                                            newGoal = subst->>(cond++rest)
                                                                                            subTree = sld strategy prog newGoal
                                                                                        in case newGoal of
                                                                                            Goal [] -> Just (subst,Success)
                                                                                            _       -> Just (subst,subTree)

    callSubstitution::BuildInRule
    callSubstitution strategy prog (Goal (term@(Comb "call" (Comb a args:restArgs)):restGoal)) = Just (Subst[],sld strategy prog (Goal (Comb a (args++restArgs):restGoal)))
    callSubstitution _ _    _                                  = Nothing

    evalSubstitution::BuildInRule
    evalSubstitution _        prog (Goal [Comb "is" [Var i, term]])     |  Just (Left  a) <- eval term = Just (single i (Comb (              show a) []), Success)
                                                                        |  Just (Right a) <- eval term = Just (single i (Comb (map toLower $ show a) []), Success)
    evalSubstitution strategy prog (Goal (Comb "is" [Var i,term]:rest)) |  Just (Left  a) <- eval term = Just (single i (Comb (              show a) []), sld strategy prog (Goal rest))
                                                                        |  Just (Right a) <- eval term = Just (single i (Comb (map toLower $ show a) []), sld strategy prog (Goal rest))
    evalSubstitution _        _    _                                                                   = Nothing

    notSubstitution::BuildInRule
    notSubstitution _ _ _= Nothing

    {-
        given a Rule and a Goal will replace all Variables in then rule that are also in then Goal by new ones
    -}
    (><)::Rule->Goal->Rule
    (><) (pat :- cond) (Goal terms) = let
                                        --list of used Variables in then Goal
                                        usedGoal    = concatMap varsInUse terms
                                        --list of usedGoal Variables in then Pattern
                                        usedRule    = concatMap varsInUse (pat:cond)
                                        --list of unusedVariables
                                        notUsed = [ x | x <- [0,1..], x `notElem` usedGoal, x `notElem` usedRule]
                                        --create a Substitution for creating then new Rule
                                        subst   = Subst [(i,Var (notUsed !! i))| i<-usedGoal ]
                                        --creating pattern and condition for new Rule
                                        newPat  = apply subst pat
                                        (Goal newCond) = subst->>cond
                                      in
                                        newPat :- newCond
