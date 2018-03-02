module SLD(sld,predefinedRules) where
    import Data.Char
    import Data.Maybe

    import Lib
    import Substitution
    import Unifikation

    --TODO document more

    predefinedRulesMap :: [(String, BuildInRule)]
    predefinedRulesMap = [ ("call", callSubstitution)
                         , ("is", evalSubstitution)
                         , ("not", notSubstitution "not")
                         , ("\\+", notSubstitution "\\+")
                         , ("findall", findAllSubstitution)
                         ]

    predefinedRules :: [Rule]
    predefinedRules = [Comb x [] :- [] | (x, _) <- predefinedRulesMap]

    {-|
        Using the FIRTS selection strategy given a Program and a Goal
        this function will produce the corresponding SLDTree
    -}
    sld :: Strategy -> Prog -> Goal -> SLDTree
    sld _        _                   (Goal [])
        = Success
    sld strategy program@(Prog rules) goal
        = SLDTree $ catMaybes   [substitute rule strategy program goal
                                | rule <- predefinedRules++rules]
          where
            substitute :: Rule->BuildInRule
            substitute rule@(Comb a _ :- _) strategy prog goal
                | Just func <- lookup a predefinedRulesMap
                = func strategy prog goal
                | otherwise
                = baseSubstitution rule strategy prog goal

    baseSubstitution :: Rule -> BuildInRule
    -- |this should never happen
    baseSubstitution _    _        _    (Goal [])
        =                       Nothing
    baseSubstitution rule strategy prog goal@(Goal (term:rest))
        = let (pat :- cond) = rule >< goal in
            case unify term pat of
                Nothing     ->  Nothing
                Just subst  ->
                    let
                        newGoal = subst ->> (cond ++ rest)
                        subTree = sld strategy prog newGoal
                    in          Just (subst, subTree)


    callSubstitution :: BuildInRule
    callSubstitution strategy prog (Goal (Comb _ (Comb a args:restArgs):restGoal))
        = Just  ( Subst[]
                , sld strategy prog (Goal (Comb a (args ++ restArgs):restGoal))
                )
    callSubstitution _        _    _
        = Nothing

    evalSubstitution :: BuildInRule
    evalSubstitution strategy prog (Goal (Comb _ [Var i, term]:rest))
                    | Just just <- eval term
                    =   let
                            substitution = single i $ case just of
                                Left  a -> Comb (              show a) []
                                Right a -> Comb (map toLower $ show a) []
                            subTree = sld strategy prog (Goal rest)
                        in  Just (substitution,subTree)
    evalSubstitution _        _    _
                    =       Nothing


    notSubstitution :: String -> BuildInRule
    notSubstitution opCode strategy prog (Goal (Comb _ goal:rest))
        =  case strategy $ sld strategy prog (Goal goal) of
             [] ->  Just (empty, sld strategy prog (Goal rest))
             _  ->  Nothing
    notSubstitution _      _        _    _
        =           Nothing

    findAllSubstitution :: BuildInRule
    findAllSubstitution strategy prog (Goal (Comb _ [template, called, Var index]:rest))
        =   let
                results = strategy $ sld strategy prog (Goal [called])
                subTree = sld strategy prog (Goal rest)
                --TODO for each template instance replace the free variables with new unused ones
                bag = hListToPList (map (`apply` template) results)
            in
                Just (Subst [(index,bag)], subTree)
    findAllSubstitution _        _     _
        =       Nothing

    {-|
        Converts a haskell list of Terms to a Prolog List of Terms
    -}
    hListToPList::[Term]->Term
    hListToPList []          = Comb "[]" []
    hListToPList (head:tail) = Comb "." [head,hListToPList tail]

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
