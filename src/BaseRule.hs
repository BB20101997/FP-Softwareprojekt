{-|
    This module defines a function for
    converting Prolog Rules to a RuleApplicator
-}
module BaseRule(baseSubstitution) where
    import qualified Lib
    import qualified Unifikation as Uni
    import Lib(Rule(..), RuleApplicator, Goal(..), Term(..))
    import Substitution((->>), (><))

    {-|
        Converts a Prolog Rule into a RuleApplicator function
    -}
    baseSubstitution :: Rule -> RuleApplicator
    -- this should never happen
    baseSubstitution _    _ (Goal [])
        =                       Nothing
    baseSubstitution rule p (Goal (term:rest))
        | (Comb _ _ :- _) <- rule
        , (Comb _ _) <- term
        = let (pat :- cond) = rule >< Lib.usedVars p in
            case Uni.unify term pat of
                Nothing     ->  Nothing
                Just subst  -> let goal' = subst ->> (cond ++ rest) in
                                Just (subst, goal')
        | otherwise
        = Nothing  -- This would throw an error in SWI-Prolog
