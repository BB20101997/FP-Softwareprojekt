module SLD where
    import Type
    import Data.Maybe
    import Lib
    import Substitution
    import Unifikation
    import Pretty
    import Data.Bifunctor

    data SLDTree = SLDTree [(Subst, SLDTree)] |  Success
                deriving Show

    instance Pretty SLDTree where
        prettyWithVars _  Success = "Success"
        prettyWithVars v (SLDTree stuff)   = "SLDTree ["++ prettyWithVars v stuff++"]"

    unpack::SLDTree->[(Subst,SLDTree)]
    unpack (SLDTree pack) = pack

    call::Rule
    call = Comb "call" [] :- []

    is::Rule
    is = Comb "is" [] :- []

    predefinedRules::[Rule]
    predefinedRules = [call,is]

    {-
        For a given Program and Goal produces the corresponding SLDTree based on FIRST Selection-strategy
    -}
    sld::Prog->Goal->SLDTree
    sld program@(Prog rules) goal = SLDTree $ catMaybes [substitute program rule goal | rule <- predefinedRules++rules]
        where
            substitute::Prog->Rule->Goal->Maybe (Subst,SLDTree)
            substitute _    _    (Goal [])               = Just (empty,Success)
            substitute prog (Comb "call"  _ :- _) goal   = callSubstitution prog goal  --higher Order Predicates
            substitute prog (Comb "is"    _ :- _) goal   = evalSubstitution prog goal  --is/eval
            substitute prog rule goal@(Goal (term:rest)) =  let
                                                                (pat :- cond)      = rule >< goal
                                                                unifier            = unify term pat
                                                            in case unifier of
                                                                Nothing     -> Nothing
                                                                Just subst  ->  let
                                                                                    newGoal = subst->>(cond++rest)
                                                                                    subTree = sld prog newGoal
                                                                                in case newGoal of
                                                                                    Goal [] -> Just (subst,Success)
                                                                                    _       -> Just (subst,subTree)

    callSubstitution::Prog->Goal->Maybe (Subst,SLDTree)
    callSubstitution prog (Goal (term@(Comb "call" (Comb a args:restArgs)):restGoal)) = Just (Subst[],sld prog (Goal (Comb a (args++restArgs):restGoal)))
    callSubstitution _    _                                  = Nothing

    evalSubstitution::Prog->Goal->Maybe (Subst,SLDTree)
    evalSubstitution prog (Goal (Comb "is" [Var i,term]:rest)) |  Just (Left a)  <- eval term = Just (single i (Comb (show a) []),sld prog (Goal rest))
                                                               |  Just (Right a) <- eval term = Just (single i (Comb (show a) []),sld prog (Goal rest))
    evalSubstitution _    _                                                                   = Nothing

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
