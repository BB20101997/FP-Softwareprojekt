{-|
    This Module Provides a few pre-defined BuildInRules
    and their RuleApplicator functions
-}
module Rule (predefinedRules) where
    import qualified Text.Read as Read

    import qualified Lib
    import qualified Substitution as Subst
    import qualified Unifikation  as Uni
    import qualified BaseRule
    import qualified SLD'(sld)
    import Lib  ( BuildInRule, Rule(..), RuleApplicator
                , Goal(..), Term(..), VarIndex
                )
    import Substitution((->>), (><))

    commaRule :: Rule
    commaRule = Lib.Comb "," [Var 0, Var 1] :- [Var 0, Var 1]

    semicolonRule1 :: Rule
    semicolonRule1 = Lib.Comb ";" [Var 0, Var 1] :- [Var 0]

    semicolonRule2 :: Rule
    semicolonRule2 = Lib.Comb ";" [Var 0, Var 1] :- [Var 1]

    -- | A Map of of a Name to a BuildInRule
    predefinedRules :: [BuildInRule]
    predefinedRules
        =   [   ( callSubstitution
                , generatePrologRule "call"    1
                )
            ,   ( isSubstitution
                , generatePrologRule "is"      2
                )
            ,   ( findAllSubstitution
                , generatePrologRule "findall" 3
                )
            ,   ( notSubstitution "not"
                , generatePrologRule "not"     1
                )
            ,   ( notSubstitution "\\+"
                , generatePrologRule "\\+"     1
                )
            ,   ( comparativeSubstitution "=:="  (==)
                , generatePrologRule "=:="     2
                )
            ,   ( comparativeSubstitution "=\\=" (/=)
                , generatePrologRule "=\\="    2
                )
            ,   ( comparativeSubstitution "<"    (< )
                , generatePrologRule "<"       2
                )
            ,   ( comparativeSubstitution ">"    (> )
                , generatePrologRule ">"       2
                )
            ,   ( comparativeSubstitution ">="   (>=)
                , generatePrologRule ">="      2
                )
            ,   ( comparativeSubstitution "=<"   (<=)
                , generatePrologRule "=<"      2
                )
            ,   ( BaseRule.baseSubstitution commaRule
                , commaRule
                )
            ,   ( BaseRule.baseSubstitution semicolonRule1
                , semicolonRule1
                )
            ,   ( BaseRule.baseSubstitution semicolonRule2
                , semicolonRule2
                )
            ]

    {-|
        generates a Stub Prolog Rule for the given name and Parameter Count
    -}
    generatePrologRule :: String -> Int -> Rule
    generatePrologRule name paramCount
        = Lib.Comb name  [Var r | r <- [1 .. paramCount]] :- []

    {-|
       The Substitution function for the BuildInRule call
    -}
    callSubstitution :: RuleApplicator
    callSubstitution _ (Goal (Comb "call" (Comb op as:bs):rgs))
        = Just  (Subst.empty, Goal (Comb op (as ++ bs) : rgs))
    callSubstitution _ _
        = Nothing

    {-|
       The Substitution function for the BuildInRule is
    -}
    isSubstitution :: RuleApplicator
    isSubstitution _ (Goal (Comb "is" [v, term] : rest))
        | Just just <- eval term
        =   let
                result = Uni.unify v $ Comb (show just) []
            in
                fmap (\s -> (s, s->>rest)) result
    isSubstitution _ _
        =                               Nothing

    {-|
        evaluates an arithmetic Prolog Term
    -}
    eval :: Term -> Maybe Int
    eval (Comb a [])        = Read.readMaybe a
    eval (Comb op [a, b])   | Just a' <- eval a
                            , Just b' <- eval b
                            = evalInt op a' b'
    eval _                  = Nothing

    {-|
        evaluated an arithmetic expression
    -}
    evalInt :: String -> Int -> Int -> Maybe Int
    evalInt op a b | op == "+"             = Just $ a + b
                   | op == "-"             = Just $ a - b
                   | op == "*"             = Just $ a * b
                   | op == "div" && b /= 0 = Just $ a `div` b
                   |otherwise              = Nothing

    {-|
        Takes an operator name and an operator function
    -}
    comparativeSubstitution :: String -> (Int -> Int -> Bool) -> RuleApplicator
    comparativeSubstitution opC op _ (Goal (Comb opC' [a, b]:rest))
        | opC == opC'
        , Just x <- eval a
        , Just y <- eval b
        , op x y
        = Just (Subst.empty,Goal rest)
    comparativeSubstitution _   _ _ _
        = Nothing

    {-|
        evaluated a negation
    -}
    notSubstitution :: String->RuleApplicator
    notSubstitution opCode p (Goal (Comb op [goal]:rest))
        | opCode == op, [] <- Lib.usedStrategy p (SLD'.sld p (Goal [goal]))
        = Just (Subst.empty, Goal rest)
    notSubstitution _      _ _
        = Nothing

    {-|
        evaluates a findall predicate
    -}
    findAllSubstitution :: RuleApplicator
    findAllSubstitution p (Goal (Comb "findall" [template, called, bag]:rs))
        = let
            results    = Lib.usedStrategy p $ SLD'.sld p (Goal [called])
            instances  = map (`Subst.apply` template) results
            instances' = newFreeVariables (Lib.usedVars p) instances
            resultBag  = hListToPList instances'
          in case Uni.unify bag resultBag of
            Just s  ->  Just (s, s ->> rs)
            Nothing     ->  Nothing
    findAllSubstitution _ _
        =                   Nothing

    {-|
        replaces the free variables in a List of Terms with new unique once
    -}
    newFreeVariables :: [VarIndex] -> [Term] -> [Term]
    newFreeVariables _ []     = []
    newFreeVariables v (x:xs) = let
                                    x' :- _ = x :- [] >< v
                                    v' = v ++ Lib.varsInUse x'
                                in
                                    x':newFreeVariables v' xs

    {-|
        Converts a haskell list of Terms to a Prolog List of Terms
    -}
    hListToPList :: [Term] -> Term
    hListToPList []     = Comb "[]" []
    hListToPList (x:xs) = Comb "." [x, hListToPList xs]
