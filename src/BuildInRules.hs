{-|
    This Module Provides a few pre-defined BuildInRules
    and a function for converting Prolog Rules to BuildInRules
-}
module BuildInRules(baseSubstitution,predefinedRules,predefinedRulesMap) where
    import Data.Char
    import Text.Read

    import Lib
    import Substitution
    import Unifikation

    -- | A Map of of a Name to a BuildInRule
    predefinedRulesMap :: [(String, BuildInRule)]
    predefinedRulesMap = [ ("call", callSubstitution)
                         , ("is", evalSubstitution)
                         , ("not", notSubstitution "not")
                         , ("\\+", notSubstitution "\\+")
                         , ("findall", findAllSubstitution)
                         ]

    -- | A Prolog Rule for each predefined BuildInRule
    predefinedRules :: [Rule]
    predefinedRules = [Comb x [] :- [] | (x, _) <- predefinedRulesMap]

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

    {-|
       The Substitution function for the BuildInRule call
    -}
    callSubstitution :: BuildInRule
    callSubstitution _ strategy prog (Goal (Comb "call" (Comb op as:bs):rgs))
        = Just  (Subst [], Goal (Comb op (as ++ bs) : rgs))
    callSubstitution _ _        _    _
        = Nothing

    {-|
       The Substitution function for the BuildInRule is
    -}
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

    {-|
        evaluates an arithmetic/boolean Prolog Term
    -}
    eval :: Term -> Maybe (Either Int Bool)
    eval (Comb a []) = case (readMaybe :: String -> Maybe Int) a of
                            Just int -> Just $ Left int
                            Nothing  -> case (readMaybe :: String -> Maybe Bool) a of
                                                Just bool -> Just $ Right bool
                                                Nothing   -> Nothing
    eval (Comb op [t1, t2])    | (Just (Left a), Just (Left b)) <-(eval t1, eval t2)  = evalInt op a b
                               | (Just (Right a), Just (Right b)) <-(eval t1, eval t2) = evalBool op a b
    eval _ = Nothing

    {-|
        evaluated an arithmetic expression
    -}
    evalInt :: String -> Int -> Int -> Maybe (Either Int Bool)
    evalInt op a b | op == "+"             = Just $ Left  $ a+b
                   | op == "-"             = Just $ Left  $ a-b
                   | op == "*"             = Just $ Left  $ a*b
                   | op == "div" && b /= 0 = Just $ Left  $ a `div` b
                   | op == "<"             = Just $ Right $ a < b
                   | op == ">"             = Just $ Right $ a > b
                   | op == "<="            = Just $ Right $ a <= b
                   | op == ">="            = Just $ Right $ a >= b
                   | op == "=:="           = Just $ Right $ a == b
                   | op == "=\\="          = Just $ Right $ a /= b
                   |otherwise              = Nothing

    {-|
        evaluates a boolean comparison
    -}
    evalBool :: String -> Bool -> Bool -> Maybe(Either Int Bool)
    evalBool op a b | op == "=:="  = Just $ Right $ a==b
                    | op == "=\\=" = Just $ Right $ a/=b
                    | otherwise    = Nothing


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
