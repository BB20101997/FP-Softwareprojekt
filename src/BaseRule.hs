{-|
    This module defines a function for
    converting Prolog Rules to a RuleApplicator
-}
module BaseRule(baseSubstitution, buildInToPrologRule) where
    import qualified Lib
    import qualified Unifikation as Uni
    import Lib(Rule(..), RuleApplicator, Goal(..), BuildInRule, Term(..))
    import Substitution((->>), (><))

    {-|
        Converts a Prolog Rule into a RuleApplicator function
    -}
    baseSubstitution :: Rule -> RuleApplicator
    -- this should never happen
    baseSubstitution _    _ (Goal [])
        =                       Nothing
    baseSubstitution rule p (Goal (term:rest))
        = let (pat :- cond) = rule >< Lib.usedVars p in
            case Uni.unify term pat of
                Nothing     ->  Nothing
                Just subst  -> let goal' = subst ->> (cond ++ rest) in
                                Just (subst, goal')

    -- | A Prolog Rule for each predefined BuildInRule
    buildInToPrologRule :: [BuildInRule] -> [Rule]
    buildInToPrologRule rules = [Comb x [] :- [] | (x, _) <- rules]
