{-|
    This module handles creating an SLDTree from a Strategy, Program and Goal
-}
module SLD(sld) where
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
    sld' param@SLDParameter { Lib.usedProgram = Prog rules
                            , Lib.usedBuildIn = preRules
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
                    rule' = baseSubstitution rule
                    -- find a build in function or use rule'
                    func  = Maybe.fromMaybe rule' (lookup op preRules)
                  in
                    -- apply ruleSubstitution
                    func sld' param goal

                    {-|
        Converts a Prolog Rule into a RuleApplicator function
    -}
    baseSubstitution :: Rule -> RuleApplicator
    -- |this should never happen
    baseSubstitution _    _ _ (Goal [])
        =                       Nothing
    baseSubstitution rule s p (Goal (term:rest))
        = let (pat :- cond, v') = rule >< usedVars p in
            case Uni.unify term pat of
                Nothing     ->  Nothing
                Just subst  -> let goal' = subst ->> (cond ++ rest) in
                                Just (subst, s p{usedVars = v'} goal')
