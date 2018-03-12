{-|
    This Module implements a BFS Strategy
    and a DFS Strategy for finding solutions in an SLDTree
-}
module Strategy(dfs, bfs) where
    import qualified Data.Bifunctor as Bi
    import qualified Data.List as List

    import qualified Substitution as Subst
    import Lib(Strategy, Subst, SLDTree(..))

    -- | applies a Strategy to a single resolution
    dfsMap :: (Subst, SLDTree) -> [Subst]
    dfsMap (substitution, Success)
        = [substitution]
    dfsMap  xs
        = uncurry map $ Bi.bimap (flip Subst.compose) dfs xs

    -- |Performs a depth-first-search on an SLDTree searching for solutions
    dfs :: Strategy
    dfs (SLDTree resolutions)
        = concatMap dfsMap resolutions
    dfs Success
        = [Subst.empty]

    -- |Performs a breath-first-search on an SLDTree searching for solution
    bfs :: Strategy
    bfs Success                 = [Subst.empty]
    bfs (SLDTree [])            = []
    bfs tree@(SLDTree subst)    = [s | (s, Success) <- subst] ++ bfs (step tree)

    {-|
        Performs a step of execution for each resolution
        Success and Empty Subtrees are removed
        and for all other SubTrees the performed substitution
        will be composed onto their subtrees substitution
        returning a new SLDTree
        of the resulting  SubSubTrees and their substitutions
    -}
    step :: SLDTree -> SLDTree
    step (SLDTree tree)
        = SLDTree   [ (Subst.compose subSubst subst, subTree)
                    | (subst                       , SLDTree subTrees) <- tree
                    , (subSubst                    , subTree) <- subTrees
                    ]
