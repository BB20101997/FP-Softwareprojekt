{-|
    This module handles creating an SLDTree from a Strategy, Program and Goal
-}
module SLD(sld, predefinedRules) where
    import Data.Maybe
    import Data.Bifunctor

    import Lib
    import BuildInRule

    {-|
        Using the FIRST selection strategy given a Program and a Goal
        this function will produce the corresponding SLDTree
    -}
    sld :: SLD
    sld _        _                   (Goal [])
        = Success
    sld strategy prog@(Prog rules) goal
        = SLDTree $ catMaybes   [substitute rule
                                |rule <- predefinedRules ++ rules]
          where
            -- | tries to perform a substitution for the given rule
            substitute :: Rule -> Maybe (Subst, SLDTree)
            substitute (Var _ :- _)
                = Nothing
            substitute rule@(Comb op _ :- _)
                = let
                    -- in case it is not build in create a build in rule
                    rule' = baseSubstitution rule
                    -- find a build in function or use rule'
                    func = fromMaybe rule' (lookup op predefinedRulesMap)
                    -- apply to Maybe get a Substitution and the remaining goal
                    result = func sld strategy prog goal
                  in
                    -- recurs on the remaining goal
                    fmap (second $ sld strategy prog) result
