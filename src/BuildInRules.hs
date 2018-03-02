module BuildInRules(predefinedRules,predefinedRulesMap) where
    import Data.Char

    import Lib
    import Substitution

    predefinedRulesMap :: [(String, BuildInRule)]
    predefinedRulesMap = [ ("call", callSubstitution)
                         , ("is", evalSubstitution)
                         , ("not", notSubstitution "not")
                         , ("\\+", notSubstitution "\\+")
                         , ("findall", findAllSubstitution)
                         ]

    predefinedRules :: [Rule]
    predefinedRules = [Comb x [] :- [] | (x, _) <- predefinedRulesMap]

    callSubstitution :: BuildInRule
    callSubstitution _ strategy prog (Goal (Comb "call" (Comb a args:restArgs):restGoal))
        = Just  (Subst [], Goal (Comb a (args ++ restArgs) : restGoal))
    callSubstitution _ _        _    _
        = Nothing

    evalSubstitution :: BuildInRule
    evalSubstitution _ strategy prog (Goal (Comb "is" [Var i, term]:rest))
                    | Just just <- eval term
                    =   let
                            substitution = single i $ case just of
                                Left  a -> Comb (              show a) []
                                Right a -> Comb (map toLower $ show a) []
                        in  Just (substitution,Goal rest)
    evalSubstitution _ _        _    _
                    =       Nothing


    notSubstitution :: String->BuildInRule
    notSubstitution opCode sld strategy prog (Goal (Comb op goal:rest))
        | opCode == op
        =  case strategy (sld strategy prog (Goal goal)) of
             [] ->  Just (empty, Goal rest)
             _  ->  Nothing
    notSubstitution _      _   _        _    _
        =           Nothing

    findAllSubstitution :: BuildInRule
    findAllSubstitution sld strategy prog (Goal (Comb "findall" [template, called, Var index]:rest))
        =   let
                results = strategy $ sld strategy prog (Goal [called])
                --TODO for each template instance replace the free variables with new unused ones
                bag = hListToPList (map (`apply` template) results)
            in
                Just (Subst [(index,bag)], Goal rest)
    findAllSubstitution _   _        _     _
        =       Nothing

    {-|
        Converts a haskell list of Terms to a Prolog List of Terms
    -}
    hListToPList::[Term]->Term
    hListToPList []          = Comb "[]" []
    hListToPList (head:tail) = Comb "." [head,hListToPList tail]
