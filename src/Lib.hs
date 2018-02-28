module Lib where
    import Type
    import Parser
    import Text.Read

    instance (Eq Term) where
        (==) (Var i) (Var i2) = i == i2
        (==) (Comb s r) (Comb s2 r2) | s==s2 = all (uncurry (==)) (zip r r2)
        (==) _ _ = False

    isIn::VarIndex->Term->Bool
    isIn a (Var b) = a==b
    isIn a (Comb b tb) = any (isIn a) tb

    varsInUse::Term->[Int]
    varsInUse (Var v)        = [v]
    varsInUse (Comb _ terms) = [ x |term<-terms, x<-varsInUse term]

    eval::Term->Maybe (Either Int Bool)
    eval (Comb a []) = case (readMaybe::String -> Maybe Int) a of
                            Just int -> Just $ Left int
                            Nothing  -> case (readMaybe::String -> Maybe Bool) a of
                                                Just bool -> Just $ Right bool
                                                Nothing   -> Nothing
    eval (Comb op [t1,t2])    | (Just (Left a),Just (Left b)) <-(eval t1,eval t2)  = evalInt op a b
                              | (Just (Right a),Just (Right b)) <-(eval t1, eval t2) = evalBool op a b
    eval _ = Nothing

    evalInt::String->Int->Int->Maybe(Either Int Bool)
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

    evalBool::String->Bool->Bool->Maybe(Either Int Bool)
    evalBool op a b | op == "=:="  = Just $ Right $ a==b
                    | op == "=\\=" = Just $ Right $ a/=b
                    | otherwise    = Nothing

    {-If order is implemented for chars and list shouldn't nub
     and merge work with List of List as they work with order Types? -}
    nubString::[String] -> [String] -- Semms like it dosen't like the implemented Nub for lists for lists of lists
    nubString (l) = nub' l []
                    where
                         nub' [] _           = []
                         nub' (x:xs) ls | x `elem` ls   = nub' xs ls
                                        | otherwise     = x : nub' xs (x:ls)

    fsthalf :: [String] -> [String]
    fsthalf xs = take (length xs `div` 2) xs

    sndhalf :: [String] -> [String]
    sndhalf xs = drop (length xs `div` 2) xs


    mergeString ::[String] -> [String] -> [String]
    mergeString xs [] = xs
    mergeString [] ys = ys
    mergeString (x:xs) (y:ys)   | (x <= y)  = x:(mergeString xs (y:ys))
                                | otherwise = y:(mergeString (x:xs) ys)





    mergesortString:: [String] -> [String]
    mergesortString [] = []
    mergesortString [x] = [x]
    mergesortString xs = mergeString (mergesortString (fsthalf xs)) (mergesortString (sndhalf xs))
