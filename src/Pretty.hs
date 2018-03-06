{-|
    Defines the Pretty class and a few instances
-}
module Pretty where
    import Data.Maybe(fromMaybe, fromJust)

    import Type(VarIndex, Term(..))

    -- |If no Variable names are specified pretty uses these as default
    defaultVarNames :: [(VarIndex, String)]
    defaultVarNames = [(i, 'A':show i) | i <- [0 ..]]

    -- | Show but pretty and with optional Variable names
    class Pretty a where
        -- |Converts a type into a String with generic Variable names
        pretty :: a -> String
        pretty = prettyWithVars defaultVarNames

        -- |Converts a type into a String with specified Variable names
        prettyWithVars :: [(VarIndex, String)] -> a -> String
        prettyWithVars _ = pretty

    -- |A Pretty Instance for Terms with special handling of Prolog Lists
    instance (Pretty Term) where
        prettyWithVars v (Var x)
            = fromMaybe (fromJust $ x `lookup` defaultVarNames) (x `lookup` v)
        prettyWithVars v (Comb "." [x, Comb "[]" []])
            = '[':prettyWithVars v x ++ "]"
        prettyWithVars v (Comb "." [x, r@(Comb "." [_, _])])
            = '[':prettyWithVars v x ++ ", " ++ tail (prettyWithVars v r)
        prettyWithVars v (Comb "." [x, r])
            = '[':prettyWithVars v x ++ '|': prettyWithVars v r ++ "]"
        prettyWithVars v (Comb "," [x, r@(Comb "," [_, _])])
          = '(':prettyWithVars v x ++ ", " ++ tail (prettyWithVars v r)
        prettyWithVars v (Comb "," [x, y])
          = '(':prettyWithVars v x ++ ", " ++ prettyWithVars v y ++ ")"
        prettyWithVars _ (Comb y [])
            = y
        prettyWithVars v (Comb y [x])
            = y ++ '(':prettyWithVars v x ++ ")"
        prettyWithVars v (Comb y (x:xs))
            = y ++ '(':prettyWithVars v x ++ ", " ++ prettyWithVars v xs ++ ")"

    -- |Pretty for Pretty Arrays
    instance (Pretty b) => (Pretty [b]) where
        prettyWithVars _ []
            = ""
        prettyWithVars v [x]
            = prettyWithVars v x
        prettyWithVars v (x:xs)
            = prettyWithVars v x ++ ", " ++ prettyWithVars v xs

    -- |We wanted Pretty Tuples somewhere, here we got them
    instance (Pretty a, Pretty b) => Pretty (a, b) where
        prettyWithVars v (a, b)
            = '(':prettyWithVars v a ++ ", " ++ prettyWithVars v b ++ ")"
