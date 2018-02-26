{-# LANGUAGE TemplateHaskell #-}
module SubstTest where
    import Type
    import Substitution
    import Test.QuickCheck

    instance (Arbitrary Term) where
        arbitrary = do
                        a <- arbitrary
                        if a then
                                do
                                    vIndex <- arbitrary
                                    return (Var vIndex)
                            else
                                do
                                    tList <- arbitrary
                                    tName <- arbitrary
                                    return (Comb tName tList)



    --TODO Arbitrary Instance für Term
    substTest::Subst -> Subst -> Term -> Bool
    substTest s1 s2 t = (apply (compose s1 s2) t) == (apply s2 (apply s1 t))

    return[]
    runTests = $quickCheckAll
