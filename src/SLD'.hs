{-|
    This module handles creating an SLDTree from an SLDParameter and a Goal
-}
module SLD'(sld) where
    import qualified Data.Maybe as Maybe
    import qualified Data.List as List(nub)

    import BaseRule(baseSubstitution)
    import qualified Lib
    import Lib  ( SLDParameter(..), Goal(..), Prog(..)
                , Subst, SLDTree(..), RuleApplicator
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
                [substitute r'
                | r' <- [r                  | (r,_)<- preRules]
                     ++ [baseSubstitution r |  r   <- rules   ]
                ]
          where
            -- | tries to perform a substitution for the given rule
            substitute :: RuleApplicator -> Maybe (Subst, SLDTree)
            substitute rule
                | Just (subst, goal') <- rule param goal
                =   let
                        v' = List.nub $ v ++ Lib.varsInGoal goal'
                        param' = param{usedVars = v'}
                    in
                        Just (subst, SLD'.sld param' goal')
                | otherwise
                = Nothing
