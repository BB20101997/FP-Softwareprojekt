{-# LANGUAGE TemplateHaskell #-}
module SubstTest where
    import Debug.Trace
    import Type
    import Pretty
    import Lib
    import Substitution
    import Test.QuickCheck

    instance (Arbitrary Subst) where
        arbitrary = do
                        size <- choose (0,5)
                        tupelVector <- vectorOf size arbitrary
                        return (Subst tupelVector)

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
                                    nameLength <- elements [1..5]
                                    tName <- vectorOf nameLength (choose ('a','z'))
                                    return (Comb tName tList)

    prop_substTest::Subst -> Subst -> Term -> Bool
    prop_substTest s1 s2 t = (apply (compose s1 s2) t) == (apply s2 (apply s1 t))

    return[]
    runTests = $quickCheckAll
