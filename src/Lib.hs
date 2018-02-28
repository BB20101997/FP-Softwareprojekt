module Lib where
    import Type

    instance (Eq Term) where
        (==) (Var i) (Var i2) = i == i2
        (==) (Comb s r) (Comb s2 r2) | s==s2 = foldl (&&) True (map (uncurry (==)) (zip r r2))
        (==) _ _ = False

    isIn::VarIndex->Term->Bool
    isIn a (Var b) = a==b
    isIn a (Comb b tb) = foldl (||) False (map (isIn a) tb)

    varsInUse::Term->[Int]
    varsInUse (Var v)        = [v]
    varsInUse (Comb _ terms) = [ x |term<-terms, x<-(varsInUse term)]

    {-
        A function that maps the content of a Just and preserves Nothing
    -}
    maybeMap::(a->b)->(Maybe a)->(Maybe b)
    maybeMap _ Nothing = Nothing
    maybeMap f (Just a) = Just (f a)

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