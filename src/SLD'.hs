{-|
    This module handles creating an SLDTree from an SLDParameter and a Goal
-}
module SLD'(sld) where
    import qualified Data.Maybe as Maybe
    import qualified Data.List as List(nub)

    import BaseRule(buildInToPrologRule,baseSubstitution)
    import qualified Lib
    import Lib  ( SLDParameter(..), Goal(..), Prog(..)
                , Subst, SLDTree(..), Term(..), Rule(..)
                )
    {-|
        Using the FIRST selection strategy and given an SLDParameter and a Goal
        this function will produce the corresponding SLDTree
    -}
    sld :: SLDParameter -> Goal -> Lib.SLDTree
    sld _                      (Goal [])
        = Success
    sld param@SLDParameter  { Lib.usedProgram   = Prog rules
                            , Lib.usedBuildIn   = preRules
                            , Lib.usedVars      = v
                            }   goal
        = SLDTree $ Maybe.catMaybes
                    [substitute r | r <- buildInToPrologRule preRules ++ rules]
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
                    func  = Maybe.fromMaybe rule' (lookup op preRules)
                    -- apply ruleSubstitution
                    res = func param goal
                  in case res of
                    Nothing -> Nothing
                    Just (subst, goal') ->
                        let
                            v' = List.nub $ v ++ Lib.varsInGoal goal'
                            param' = param{usedVars = v'}
                        in
                            Just (subst, SLD'.sld param' goal')
