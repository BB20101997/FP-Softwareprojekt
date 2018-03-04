{-# LANGUAGE ViewPatterns #-}

{-|
    This Module Provides a few pre-defined BuildInRules
    and a function for converting Prolog Rules to BuildInRules
-}
module Rule ( buildInToPrologRule
            , predefinedRules
            ,(><)
            ) where
    import qualified Data.Char as Char
    import qualified Text.Read as Read

    import qualified Lib
    import qualified Substitution as Subst
    import qualified Unifikation as Uni
    import Lib  ( BuildInRule, Rule(..), RuleApplicator
                , Goal(..), Term(..), VarIndex
                )
    import Substitution((->>), (><))

    -- | A Map of of a Name to a BuildInRule
    predefinedRules :: [BuildInRule]
    predefinedRules = [ ("call", callSubstitution)
                         , ("is", evalSubstitution)
                         , ("not", notSubstitution "not")
                         , ("\\+", notSubstitution "\\+")
                         , ("findall", findAllSubstitution)
                         ]

    -- | A Prolog Rule for each predefined BuildInRule
    buildInToPrologRule :: [BuildInRule] -> [Rule]
    buildInToPrologRule rules = [Comb x [] :- [] | (x, _) <- rules]



    {-|
       The Substitution function for the BuildInRule call
    -}
    callSubstitution :: RuleApplicator
    callSubstitution sld p (Goal (Comb "call" (Comb op as:bs):rgs))
        = Just  (Subst.empty, sld p $ Goal (Comb op (as ++ bs) : rgs))
    callSubstitution _   _ _
        = Nothing

    {-|
       The Substitution function for the BuildInRule is
    -}
    evalSubstitution :: RuleApplicator
    evalSubstitution sld p (Goal (Comb "is" [v, term] : rest))
        | Just just <- eval term
        =   let
                result = case just of
                    Left  a -> Comb (                   show a) []
                    Right a -> Comb (map Char.toLower $ show a) []
            in case Uni.unify v result of
                Just subst  -> Just (subst,sld p $ subst ->> rest)
                Nothing     -> Nothing
    evalSubstitution _   _ _
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
    notSubstitution opCode sld p (Goal (Comb op goal:rest))
        | opCode == op, [] <- Lib.usedStrategy p (sld p (Goal goal))
        = Just (Subst.empty, sld p $ Goal rest)
    notSubstitution _      _   _ _
        = Nothing

    {-|
        evaluates a findall predicate
    -}
    findAllSubstitution :: RuleApplicator
    findAllSubstitution sld p (Goal (Comb "findall" [template, called, bag]:rs))
        = let
            results = Lib.usedStrategy p $ sld p (Goal [called])
            instances = map (`Subst.apply` template) results
            (v',instances') = newFreeVariables (Lib.usedVars p) instances
            resultBag = hListToPList instances'
          in case Uni.unify bag resultBag of
            Just s  ->  Just (s, sld p{Lib.usedVars = v'} $ s ->> rs)
            Nothing     ->  Nothing
    findAllSubstitution _   _ _
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


