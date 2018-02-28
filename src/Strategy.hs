module Strategy where
    import Type
    import Pretty
    import SLD
    import Substitution
    import Debug.Trace
    import Data.Bifunctor

    type Strategy = SLDTree -> [Subst]

    mapFunction::Strategy->(Subst        ,SLDTree) ->[Subst]
    mapFunction _          (substitution ,Success) = [substitution]
    mapFunction stratagy    input                  = uncurry map $ bimap (flip compose) stratagy input

    dfs::Strategy --TODO make it work
    dfs (SLDTree resolutions) = concatMap (mapFunction dfs) resolutions
    dfs Success = [empty]

    bfs::Strategy
{-
    bfs (SLDTree resolutions) = mapFunction bfs resolutions

-}
    bfs _ = []

