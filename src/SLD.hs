{-|
    This module handles creating an SLDTree from a Strategy, Program and Goal
-}
module SLD(sld, rulesMapToRules) where
    import Data.Maybe

    import Lib
    import BuildInRule

    {-|
        Using the FIRST selection strategy given a Program and a Goal
        this function will produce the corresponding SLDTree
    -}
    sld :: SLD
    sld strategy prog goal = sldWithVars (varsInGoal goal, strategy, prog) goal

    sldWithVars :: SLDParameter -> Goal -> SLDTree
    sldWithVars _ (Goal [])
        = Success
    sldWithVars param@(_, _, Prog rules) goal
        = SLDTree $ catMaybes [substitute r | r <- predefinedRules ++ rules]
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
                  in
                    -- apply ruleSubstitution
                    func sldWithVars param goal
