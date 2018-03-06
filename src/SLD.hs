{-|
    This module handles creating an SLDTree from a Strategy, Program and Goal
-}
module SLD(sld) where

    import qualified Lib
    import qualified Rule
    import qualified SLD'
    import Lib (Goal(..), SLDTree(..), SLDParameter(..), Prog(..), Strategy)

    {-|
        Using the FIRST selection strategy given a Program and a Goal
        this function will produce the corresponding SLDTree
    -}
    sld :: Strategy -> Prog -> Goal -> SLDTree
    sld strategy prog goal
        =   let param = SLDParameter{ Lib.usedBuildIn  = Rule.predefinedRules
                                    , Lib.usedStrategy = strategy
                                    , Lib.usedVars     = Lib.varsInGoal goal
                                    , Lib.usedProgram  = prog
                                    }
            in SLD'.sld param goal