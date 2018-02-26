{-# LANGUAGE TemplateHaskell #-}
module SubstTest where
    import Type
    import Pretty
    import ToBeNamed
    import Substitution
    import Test.QuickCheck

    instance (Arbitrary Subst) where
        arbitrary = do
                        tupelList <- arbitrary
                        return (Subst tupelList)

    instance (Show Subst) where
        show = pretty

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



    prop_substTest::Subst -> Subst -> Term -> Bool
    prop_substTest s1 s2 t = (apply (compose s1 s2) t) == (apply s2 (apply s1 t))

    return[]
    runTests = $quickCheckAll
