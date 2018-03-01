module Pretty where
    import Data.Maybe

    import Type

    -- / If no Variable names are specified pretty uses these as default
    defaultVarNames :: [(VarIndex, String)]
    defaultVarNames = [(i, 'A':show i)|i<-[0..]]

    class Pretty a where
        -- / Converts a type into a String with generic Variable names
        pretty :: a -> String
        pretty = prettyWithVars defaultVarNames

        -- / Converts a type into a String with specified Variable names
        prettyWithVars :: [(VarIndex, String)] -> a -> String
        prettyWithVars _ = pretty


    instance (Pretty Term) where
        prettyWithVars v (Var x)                = fromMaybe (fromJust $ x `lookup` defaultVarNames) (x `lookup` v)
        prettyWithVars v (Comb "." [x, Var xs]) = "[ " ++ prettyWithVars v x ++ "| " ++ prettyWithVars v (Var xs) ++ " ]"
        prettyWithVars v (Comb "." [x])         = "[ " ++ prettyWithVars v x ++ " ]"
        prettyWithVars v (Comb "." (x:xs))      = "[ " ++ prettyWithVars v x ++ ", " ++ prettyWithVars v xs ++ " ]"
        prettyWithVars v (Comb y [])            = y
        prettyWithVars v (Comb y [x])           = y ++ "( " ++ prettyWithVars v x ++ " )"
        prettyWithVars v (Comb y (x:xs))        = y ++ "( " ++ prettyWithVars v x ++ ", " ++ prettyWithVars v xs ++ " )"

    instance (Pretty b) => (Pretty [b] ) where
        prettyWithVars v []      = ""
        prettyWithVars v [x]     = prettyWithVars v x
        prettyWithVars v (x:xs)  = prettyWithVars v x ++ ", " ++ prettyWithVars v xs

    instance (Pretty a, Pretty b)=>Pretty (a, b) where
        prettyWithVars v (a, b) = '(':prettyWithVars v a ++ ", " ++ prettyWithVars v b ++ ")"
