module Unifikation(unify) where
    import Lib
    import Substitution

    {-|
        the first term is expected to be the goal
        the second term is expected to be the pattern
    -}
    ds :: Term -> Term -> Maybe (Term, Term)
    -- |already equal nothing to substitute
    ds t1              t2              | t1 == t2                = Nothing
    -- |replace goal by pattern
    ds t1@(Var _)      t2@(Var _)                                = Just (t1, t2)
    -- |t1 is var can be substituted by t2
    ds t1@(Var _)      t2@(Comb _ _)                             = Just (t1, t2)
    -- |t2 is var can be substituted by t1
    ds t1@(Comb _ _)   t2@(Var _)                                = Just (t2, t1)
    ds t1@(Comb s1 x1) t2@(Comb s2 x2)
        -- |s1 and s2 don't match all or have different parameter count
        | s1 /= s2 || length x1 /= length x2 = Just (t1, t2)
        -- |searching for first substitution of the parameters
        | otherwise               = sub_ds x1 x2
          where
            sub_ds :: [Term] -> [Term] -> Maybe (Term, Term)
            sub_ds (h1:t1) (h2:t2) | h1 == h2  = sub_ds t1 t2
                                   | otherwise = ds h1 h2

    unify :: Term -> Term -> Maybe Subst
    unify t1 t2 =
        case ds t1 t2  of
        Nothing                     ->  Just empty -- |already unified
        (Just (Var index, r)) ->
            if index `isIn` r
                then Nothing
                else
                    let uni = Subst [(index, r)] in
                    case unify (apply uni t1) (apply uni t2) of
                        -- |further substitution succeeded
                        -- , appending our substitution
                        Just set    -> Just (compose set uni)
                        Nothing     -> Nothing
        -- |replaced Term not a Variable
        (Just _)                    -> Nothing


