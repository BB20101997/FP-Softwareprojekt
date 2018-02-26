module ToBeNamed where
    import Type

    instance (Eq Term) where
        (==) (Var i) (Var i2) = i == i2
        (==) (Comb s r) (Comb s2 r2) | s==s2 = foldl (&&) True (map (uncurry (==)) (zip r r2))
        (==) _ _ = False

    isIn::VarIndex->Term->Bool
    isIn a (Var b) = a==b
    isIn a (Comb b tb) = foldl (||) False (map (isIn a) tb)


