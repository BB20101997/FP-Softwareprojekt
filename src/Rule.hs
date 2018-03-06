{-# LANGUAGE ViewPatterns #-}

{-|
    This Module Provides a few pre-defined BuildInRules
    and their RuleApplicator functions
-}
module Rule (predefinedRules) where
    import qualified Data.Char as Char
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

    commaRule::Rule
    commaRule = Lib.Comb "," [Var 0,Var 1] :- [Var 0,Var 1]

    -- | A Map of of a Name to a BuildInRule
    predefinedRules :: [BuildInRule]
    predefinedRules =   [ ("call"   , callSubstitution)
                        , ("is"     , evalSubstitution)
                        , ("not"    , notSubstitution "not")
                        , ("\\+"    , notSubstitution "\\+")
                        , ("findall", findAllSubstitution)
                        , (","      , BaseRule.baseSubstitution commaRule)
                        ]

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
    evalSubstitution :: RuleApplicator
    evalSubstitution _ (Goal (Comb "is" [v, term] : rest))
        | Just just <- eval term
        =   let
                result = Uni.unify v $ case just of
                    Left  a -> Comb (                   show a) []
                    Right a -> Comb (map Char.toLower $ show a) []
            in
                fmap (\s -> (s, s->>rest)) result
    evalSubstitution _ _
        =                               Nothing

    {-|
        evaluates an arithmetic/boolean Prolog Term
    -}
    eval :: Term -> Maybe (Either Int Bool)
    eval (Comb a [])
        | Just int  <- Read.readMaybe a     = Just $ Left int
        | Just bool <- Read.readMaybe a     = Just $ Right bool
    eval (Comb op [eval -> Just a, eval -> Just b])
        | Left  a' <- a, Left  b' <- b = evalInt  op a' b'
        | Right a' <- a, Right b' <- b = evalBool op a' b'
    eval _                             = Nothing

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
    notSubstitution :: String->RuleApplicator
    notSubstitution opCode p (Goal (Comb op goal:rest))
        | opCode == op, [] <- Lib.usedStrategy p (SLD'.sld p (Goal goal))
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