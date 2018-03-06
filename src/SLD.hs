{-|
    This module handles creating an SLDTree from a Strategy, Program and Goal
-}
module SLD(sld) where
    import qualified Data.List  as List
    import qualified Data.Maybe as Maybe

    import qualified Lib
    import qualified Rule
    import qualified Unifikation  as Uni
    import Lib ( Rule(..), Goal(..), SLDTree(..), SLDParameter(..)
               , Prog(..), Term(..), Strategy, Subst, RuleApplicator
               )
    import Substitution((->>), (><))

    {-|
        Using the FIRST selection strategy given a Program and a Goal
        this function will produce the corresponding SLDTree
    -}
    sld :: Strategy -> Prog -> Goal -> SLDTree
    sld strategy prog goal
        = sld'  SLDParameter{ Lib.usedBuildIn  = Rule.predefinedRules
                            , Lib.usedStrategy = strategy
                            , Lib.usedVars     = Lib.varsInGoal goal
                            , Lib.usedProgram  = prog
                            }
                goal

    sld' :: SLDParameter -> Goal -> Lib.SLDTree
    sld' _                      (Goal [])
        = Success
    sld' param@SLDParameter { Lib.usedProgram   = Prog rules
                            , Lib.usedBuildIn   = preRules
                            , Lib.usedVars      = v
                            }   goal
        = SLDTree $ Maybe.catMaybes [substitute r
                              | r <- Rule.buildInToPrologRule preRules ++ rules]
          where
            -- | tries to perform a substitution for the given rule
            substitute :: Rule -> Maybe (Subst, SLDTree)
            substitute (Var _ :- _)
                = Nothing
            substitute rule@(Comb op _ :- _)
                = let
                    -- in case it is not build in create a build in rule
                    rule' = Rule.baseSubstitution rule
                    -- find a build in function or use rule'
                    func  = Maybe.fromMaybe rule' (lookup op preRules)
                    -- apply ruleSubstitution
                    res = func sld' param goal
                  in case res of
                    Nothing -> Nothing
                    Just (subst, goal') ->
                        let
                            v' = List.nub $ v ++ Lib.varsInGoal goal'
                            param' = param{usedVars = v'}
                        in
                            Just (subst, sld' param' goal')

