{-# LANGUAGE TemplateHaskell #-}
module SubstTest where
    import Debug.Trace

    import Test.QuickCheck

    import Lib
    import Substitution

    instance Show Subst where
        show = pretty

    instance (Arbitrary Subst) where
        arbitrary =
            do
                size <- choose (0,5)
                singles <- vectorOf size singleGen
                return $ foldl compose empty (map  (uncurry single) singles)
            where
                singleGen :: Gen (VarIndex,Term)
                singleGen = arbitrary `suchThat` (not . uncurry isIn)

    instance (Arbitrary Term) where
        arbitrary = frequency [(75,arbitraryVar),(25,arbitraryTerm)]
            where
                arbitraryVar::Gen Term
                arbitraryVar = do
                                    (NonNegative vIndex) <- arbitrary
                                    return (Var vIndex)
                arbitraryTerm::Gen Term
                arbitraryTerm = do
                                    size <- elements [0 .. 5]
                                    tList <- vectorOf size arbitrary
                                    tName <- vectorOf 1 (choose ('a','z'))
                                    return (Comb tName tList)

    prop_substTest::Subst -> Subst -> Term -> Bool
    prop_substTest s1 s2 t = apply (compose s1 s2) t == apply s1 (apply s2 t)

    return[]
    runTests = $quickCheckAll
