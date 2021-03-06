{-|
    This module implements Unification,
    it's functionality is exposed through the unify function
-}
module Unifikation(unify) where
    import qualified Lib
    import qualified Substitution as Subst
    import Lib(Term(..),Subst(..))

    {-|
        the first term is expected to be the goal
        the second term is expected to be the pattern
        Iff the Terms are equal Nothing will be returned
        Iff the Terms can't be unified then a Just value will be
            returned where the first component is a Comb Term
        Iff the Terms are unequal and can be unified then a Just value will be
            returned where the first component is a Val Term
    -}
    ds :: Term -> Term -> Maybe (Term, Term)
    -- |already equal nothing to substitute
    ds t1              t2              | t1 == t2 = Nothing
    -- |replace var from goal by term from pattern
    ds t1@(Var _)      t2                         = Just (t1, t2)
    -- |replace var from pattern by term form goal
    ds t1@(Comb _ _)   t2@(Var _)                 = Just (t2, t1)
    ds t1@(Comb s1 x1) t2@(Comb s2 x2)
        -- s1 and s2 don't match all or have different parameter count
        | s1 /= s2 || length x1 /= length x2      = Just (t1, t2)
        -- searching for first substitution of the parameters
        | otherwise                               = sub_ds x1 x2
          where
            sub_ds :: [Term] -> [Term] -> Maybe (Term, Term)
            sub_ds (x:xs) (y:ys) | x == y    = sub_ds xs ys
                                 | otherwise = ds x y
            -- | Should never happen, but -Wall
            sub_ds _      _                  = Nothing

    {-|
       This function tries to find a Substitution
       to unify the two provided Terms.

       When replacing a Var with another
       it will keep the Var from the second Parameter,
       this means you generally want the second Parameter to be the Pattern
    -}
    unify :: Term -> Term -> Maybe Subst
    unify t1 t2 =
        case ds t1 t2  of
            -- already unified
            Nothing                 ->  Just Subst.empty
            -- replaced Term not a Variable
            (Just (Comb _ _ , _))   -> Nothing
            (Just (Var index, r))
                -- Occur Check
                | index `Lib.isIn` r    -> Nothing
                | otherwise ->
                    let uni = Subst [(index, r)] in
                    case unify (Subst.apply uni t1) (Subst.apply uni t2) of
                        --  further substitution succeeded
                        -- , appending our substitution
                        Just set    -> Just (Subst.compose set uni)
                        Nothing     -> Nothing


