module SLD(sld,predefinedRules) where
    import Data.Char
    import Data.Maybe
    import Data.Bifunctor

    import Lib
    import Substitution
    import Unifikation
    import BuildInRules

    --TODO document more
    {-|
        Using the FIRST selection strategy given a Program and a Goal
        this function will produce the corresponding SLDTree
    -}
    sld :: SLD
    sld _        _                   (Goal [])
        = Success
    sld strategy program@(Prog rules) goal
        = SLDTree $ catMaybes   [substitute rule strategy program goal
                                | rule <- predefinedRules++rules]
          where
            -- | tries to perform a substitution for the given rule
            substitute :: Rule -> Strategy -> Prog -> Goal -> Maybe (Subst,SLDTree)
            substitute rule@(Comb op _ :- _) strategy prog goal
                = let
                    -- in case it is not build in create a build in rule
                    rule' = baseSubstitution rule
                    -- find a build in function or use rule'
                    func = fromMaybe rule' (lookup op predefinedRulesMap)
                    -- apply to Maybe get a Substitution and the remaining goal
                    result = func sld strategy prog goal
                  in
                    -- recurs on the remaining goal
                    fmap (second $ sld strategy program) result

    {-|
        Converts a Prolog Rule into a BuildInRule
    -}
    baseSubstitution :: Rule -> BuildInRule
    -- |this should never happen
    baseSubstitution _    _  _        _    (Goal [])
        =                       Nothing
    baseSubstitution rule _  strategy prog goal@(Goal (term:rest))
        = let (pat :- cond) = rule >< goal in
            case unify term pat of
                Nothing     ->  Nothing
                Just subst  ->
                    let
                        goal' = subst ->> (cond ++ rest)
                    in          Just (subst, goal')

    {-|
        Produces a functionally identical Rule to the input Rule
        The resulting rule will have all rules present in the Goal
        replaced by new ones
    -}
    (><) :: Rule -> Goal -> Rule
    (><) (pat :- cond) (Goal terms) = let
                                        -- list of used Variables in then Goal
                                        usedGoal    = concatMap varsInUse terms
                                        -- list of usedGoal Variables in then Pattern
                                        usedRule    = concatMap varsInUse (pat:cond)
                                        -- list of unusedVariables
                                        notUsed = [ x | x <- [0,1..], x `notElem` usedGoal, x `notElem` usedRule]
                                        -- create a Substitution for creating then new Rule
                                        subst   = Subst [(i, Var (notUsed !! i))| i<-usedGoal ]
                                        -- creating pattern and condition for new Rule
                                        newPat  = apply subst pat
                                        (Goal newCond) = subst->>cond
                                      in
                                        newPat :- newCond
