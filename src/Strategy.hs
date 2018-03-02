{-|
    This Module implements a BFS Strategy
    and a DFS Strategy for finding solutions in an SLDTree
-}
module Strategy where
    import Data.Bifunctor
    import Data.List

    import Lib
    import Substitution

    -- | applies a Strategy to a list of resolutions
    mapStrategy :: Strategy -> [(Subst,SLDTree)] -> [[Subst]]
    mapStrategy s = map (mapFunction s)
      where
        -- | applies a Strategy to a single resolution
        mapFunction :: Strategy -> (Subst, SLDTree) -> [Subst]
        mapFunction _ (substitution , Success)
            = [substitution]
        mapFunction s xs
            = uncurry map $ bimap (flip compose) s xs

    -- |Performs a depth-first-search on an SLDTree searching for solutions
    dfs :: Strategy
    dfs (SLDTree resolutions) = concat $ mapStrategy dfs resolutions
    dfs Success = [empty]

    -- |Performs a breath-first-search on an SLDTree searching for solution
    bfs :: Strategy
    bfs (SLDTree resolutions) = concat $ transpose $ mapStrategy bfs resolutions
    bfs Success = [empty]
