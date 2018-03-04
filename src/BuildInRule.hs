{-|
    This Module Provides a few pre-defined BuildInRules
    and a function for converting Prolog Rules to BuildInRules
-}
module BuildInRule  (baseSubstitution
                    , rulesMapToRules
                    , predefinedRulesMap
                    , predefinedRules
                    ) where
    import Data.Char
    import Text.Read
    import Data.List

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

    -- | The predefinedRulesMap converted to Rules
    predefinedRules :: [Rule]
    predefinedRules = rulesMapToRules predefinedRulesMap

    -- | A Prolog Rule for each predefined BuildInRule
    rulesMapToRules :: [(String, BuildInRule)] -> [Rule]
    rulesMapToRules rules = [Comb x [] :- [] | (x, _) <- rules]

    {-|
        Converts a Prolog Rule into a BuildInRule
    -}
    baseSubstitution :: Rule -> BuildInRule
    -- |this should never happen
    baseSubstitution _    _    _        (Goal [])
        =                       Nothing
    baseSubstitution rule sld (v, s, p) (Goal (term:rest))
        = let (pat :- cond, v') = rule >< v in
            case unify term pat of
                Nothing     ->  Nothing
                Just subst  -> let goal' = subst ->> (cond ++ rest) in
                                Just (subst, sld (v', s, p) goal')

    {-|
        Produces a functionally identical Rule to the input Rule
        The resulting rule will have all rules present in the Goal
        replaced by new ones
    -}
    (><) :: Rule -> [VarIndex] -> (Rule, [VarIndex])
    (><) (pat :- cond) v
        = let
            -- list of used Variables in the Pattern
            usedR   = concatMap varsInUse (pat:cond)
            -- list of unused Variables
            notUsed = [ x | x <- [0, 1 .. ], x `notElem` v, x `notElem` usedR]
            -- already used and used in the rule
            vs = [i | i <- v, i `elem` usedR]
            -- create a Substitution for creating then new Rule
            subst   = Subst [(i, Var (notUsed !! i)) | i <- vs]
            -- new Set of used variables
            v' = nub $ v ++ usedR ++ map (notUsed !!) vs
            -- creating pattern and condition for new Rule
            pat'    = apply subst pat
            (Goal cond') = subst ->> cond
          in
            (pat' :- cond', v')

    {-|
       The Substitution function for the BuildInRule call
    -}
    callSubstitution :: BuildInRule
    callSubstitution sld p (Goal (Comb "call" (Comb op as:bs):rgs))
        = Just  (empty, sld p $ Goal (Comb op (as ++ bs) : rgs))
    callSubstitution _   _ _
        = Nothing

    {-|
       The Substitution function for the BuildInRule is
    -}
    evalSubstitution :: BuildInRule
    evalSubstitution sld p (Goal (Comb "is" [v, term] : rest))
        | Just just <- eval term
        =   let
                result = case just of
                    Left  a -> Comb (              show a) []
                    Right a -> Comb (map toLower $ show a) []
            in case unify v result of
                Just subst  -> Just (subst,sld p $ subst ->> rest)
                Nothing     -> Nothing
    evalSubstitution _   _ _
        =                               Nothing

    {-|
        evaluates an arithmetic/boolean Prolog Term
    -}
    eval :: Term -> Maybe (Either Int Bool)
    eval (Comb a [])
        | Just int  <- readMaybe a = Just $ Left int
        | Just bool <- readMaybe a = Just $ Right bool
        | otherwise                 = Nothing
    eval (Comb op [t1, t2])
        | Just a <- eval t1
        , Just b <- eval t2
        = case (a, b) of
            (Left  a', Left  b')    -> evalInt  op a' b'
            (Right a', Right b')    -> evalBool op a' b'
            _                       -> Nothing
    eval _
        =                              Nothing

    {-|
        evaluated an arithmetic expression
    -}
    evalInt :: String -> Int -> Int -> Maybe (Either Int Bool)
    evalInt op a b | op == "+"             = Just $ Left  $ a + b
                   | op == "-"             = Just $ Left  $ a - b
                   | op == "*"             = Just $ Left  $ a * b
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
    evalBool op a b | op == "=:="  = Just $ Right $ a == b
                    | op == "=\\=" = Just $ Right $ a /= b
                    | otherwise    = Nothing

    {-|
        evaluated a negation
    -}
    notSubstitution :: String->BuildInRule
    notSubstitution opCode sld p@(_, s, _) (Goal (Comb op goal:rest))
        | opCode == op
        , [] <- s (sld p (Goal goal))
        = Just (empty, sld p $ Goal rest)
    notSubstitution _      _   _           _
        =           Nothing

    {-|
        evaluates a findall predicate
    -}
    findAllSubstitution :: BuildInRule
    findAllSubstitution sld (v, s, p)
        (Goal (Comb "findall" [template, called, bag]:rest))
            = let
                results = s $ sld (v, s, p) (Goal [called])
                instances = map (`apply` template) results
                (v',instances') = newFreeVariables v instances
                resultBag = hListToPList instances'
              in case unify bag resultBag of
                Just subst  ->  Just (subst, sld (v', s, p) $ subst ->> rest)
                Nothing     ->  Nothing
    findAllSubstitution _    _     _
            =                   Nothing

    {-|
        replaces the free variables in a List of Terms with new unique once
    -}
    newFreeVariables :: [VarIndex] -> [Term] -> ([VarIndex], [Term])
    newFreeVariables v []     = (v,[])
    newFreeVariables v (x:xs) = let
                                    (x' :- _, v') = x :- [] >< v
                                    (v'', xs')   = newFreeVariables v' xs
                                in
                                    (v'', x':xs')

    {-|
        Converts a haskell list of Terms to a Prolog List of Terms
    -}
    hListToPList :: [Term] -> Term
    hListToPList []     = Comb "[]" []
    hListToPList (x:xs) = Comb "." [x, hListToPList xs]
