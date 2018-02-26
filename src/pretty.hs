import Type

pretty:: a ->String
pretty (Prog w)     = prettyProg w
pretty (Var x)      = prettyTerm (Var x)
pretty (Comb y z)   = prettyTerm (Comb y z)


prettyProg:: [Rule] ->String
prettyProg []        = ""
prettyProg (x : xs)  = (prettyRule x) ++ (prettyNextRules xs)

prettyNextRules:: [Rule] ->String
prettyNextRules []        = ""
prettyNextRules (x : xs)  = ", " ++ (pretty x) ++ (prettyNextRules xs)


prettyRule:: Term -> [Term] ->String
prettyRule a b = "true" --Testweise

prettyTerm:: Term ->String
prettyTerm Var x = prettyIndex x

prettyIndex:: VarIndex ->String
prettyIndex x = show ('A'+x)