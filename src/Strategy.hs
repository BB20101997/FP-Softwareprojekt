{-|
    This Module implements a BFS Strategy
    and a DFS Strategy for finding solutions in an SLDTree
-}
module Strategy where
    import qualified Data.Bifunctor as Bi
    import qualified Data.List as List

    import qualified Substitution as Subst
    import Lib(Strategy, Subst, SLDTree(..))

    -- | applies a Strategy to a list of resolutions
    mapStrategy :: Strategy -> [(Subst, SLDTree)] -> [[Subst]]
    mapStrategy s = map mapFunction
      where
        -- | applies a Strategy to a single resolution
        mapFunction :: (Subst, SLDTree) -> [Subst]
        mapFunction (substitution, Success)
            = [substitution]
        mapFunction  xs
            = uncurry map $ Bi.bimap (flip Subst.compose) s xs

    -- |Performs a depth-first-search on an SLDTree searching for solutions
    dfs :: Strategy
    dfs (SLDTree resolutions)
        = concat                  $ mapStrategy dfs resolutions
    dfs Success
        = [Subst.empty]

    -- |Performs a breath-first-search on an SLDTree searching for solution
    bfs :: Strategy
    bfs (SLDTree resolutions)
        = concat $ List.transpose $ mapStrategy bfs resolutions
    bfs Success
        = [Subst.empty]
