module Lib(module Lib,module Type,module Pretty) where
    import Text.Read

    import Type
    import Pretty

-- Types

    --REPL Types
    type State = (Strategy,Prog)
    type Action = State -> String -> IO ()

    --Strategy Types
    type Strategy = SLDTree -> [Subst]

    --Substitution Types
    newtype Subst = Subst [(VarIndex, Term)]

    --SLDTree Types
    type BuildInRule = Strategy -> Prog -> Goal -> Maybe (Subst, SLDTree)
    data SLDTree = SLDTree [(Subst, SLDTree)] |  Success
                deriving Show

-- Instances

    instance (Show Subst) where
       show = pretty

    instance (Pretty Subst) where
        prettyWithVars v (Subst []) = "{}"
        prettyWithVars v (Subst l)  = "{"
                                        ++ notHeadList v l
                                        ++ "}"

    instance Pretty SLDTree where
        prettyWithVars _  Success          = "Success"
        prettyWithVars v (SLDTree stuff)   = "SLDTree [" ++ prettyWithVars v stuff ++ "]"

    instance (Eq Term) where
        (==) (Var i) (Var i2) = i == i2
        (==) (Comb s r) (Comb s2 r2) | s == s2 = all (uncurry (==)) (zip r r2)
        (==) _ _ = False

--other stuff
    notHeadList ::  [(VarIndex, String)] -> [(VarIndex, Term)] -> String
    notHeadList v []          = ""
    notHeadList v (head:tail) = case (take 1 (substTupToString v head)) of
                                     "_" ->  notHeadList v tail
                                     otherwise -> substTupToString v head ++ [ x|tuple<-tail,x <- (", "++ substTupToString v tuple), notSuppress v tuple]

    notSuppress :: [(VarIndex, String)] -> (VarIndex, Term) -> Bool
    notSuppress v (head) = (take 1 (substTupToString v head)) /= "_"

    substTupToString :: [(VarIndex, String)] -> (VarIndex, Term) -> String
    substTupToString v        (index, term) = prettyWithVars v (Var index) ++ " -> " ++ prettyWithVars v term

    isIn :: VarIndex -> Term -> Bool
    isIn a (Var b) = a == b
    isIn a (Comb b tb) = any (isIn a) tb

    varsInUse :: Term -> [Int]
    varsInUse (Var v)        = [v]
    varsInUse (Comb _ terms) = [ x |term<-terms, x<-varsInUse term]

    eval :: Term -> Maybe (Either Int Bool)
    eval (Comb a []) = case (readMaybe :: String -> Maybe Int) a of
                            Just int -> Just $ Left int
                            Nothing  -> case (readMaybe :: String -> Maybe Bool) a of
                                                Just bool -> Just $ Right bool
                                                Nothing   -> Nothing
    eval (Comb op [t1, t2])    | (Just (Left a), Just (Left b)) <-(eval t1, eval t2)  = evalInt op a b
                               | (Just (Right a), Just (Right b)) <-(eval t1, eval t2) = evalBool op a b
    eval _ = Nothing

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

    evalBool :: String -> Bool -> Bool -> Maybe(Either Int Bool)
    evalBool op a b | op == "=:="  = Just $ Right $ a==b
                    | op == "=\\=" = Just $ Right $ a/=b
                    | otherwise    = Nothing


