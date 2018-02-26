module Unifikation where
    import Type
    import ToBeNamed
    import Substitution



    ds::Term -> Term -> (Maybe (Term, Term))
    ds t1              t2              | t1==t2                     = Nothing       --already equal nothing to substitute
    ds t1@(Var _)      t2                                           = Just (t1,t2)  --t1 is var can be substituted by t2
    ds t1              t2@(Var _)                                   = Just (t2,t1)  --t2 is var can be substituted by t1
    ds t1@(Comb s1 x1) t2@(Comb s2 x2) | s1/=s2                     = Just (t1,t2)  --s1 and s2 don't1 match all has to be substituted
                                       | (length x1) /= (length x2) = Just (t1,t2)  --unequal argument length all has to be substituted
                                       | otherwise                  = sub_ds x1 x2  --searching for first substitution of the parameters
        where
            sub_ds::[Term]->[Term]->Maybe (Term, Term)
            sub_ds (h1:t1) (h2:t2) | h1 == h2  = sub_ds t1 t2
                                   | otherwise = ds h1 h2

    unify::Term -> Term -> Maybe Subst
    unify t1 t2 =   case ds t1 t2  of
                        Nothing                 ->  Just empty --already unified
                        (Just ((Var index),r))  ->  if (index `isIn` r) then
                                                        Nothing -- substituted var is part of the substitution
                                                    else let
                                                            uni = (index,r)
                                                         in case unify (apply [uni] t1) (apply [uni] t2) of
                                                                Nothing -> Nothing --further substitution fails
                                                                Just set -> Just (uni:set) --further substitution succeeded appending our substitution
                        (Just _)                  -> Nothing --trying to substitute a Term for another is not a Variable


