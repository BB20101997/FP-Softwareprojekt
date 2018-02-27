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
