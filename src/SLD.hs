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

    {-
        For a given Program and Goal produces the corresponding SLDTree based on FIRST Selection-strategy
    -}
    sld::Prog->Goal->SLDTree
    sld program@(Prog rules) goal = SLDTree $ catMaybes [substitute program rule goal | rule <- rules]
        where
            substitute::Prog->Rule->Goal->Maybe (Subst,SLDTree)
            substitute _    _    (Goal [])   = Just (empty,Success)
            substitute prog rule goal        = let
                                                (Goal (term:rest)) = goal
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
