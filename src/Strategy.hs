module Strategy where
    import Data.Bifunctor

    import Lib
    import Substitution

    mapFunction :: Strategy -> (Subst, SLDTree) -> [Subst]
    mapFunction _          (substitution , Success)
        = [substitution]
    mapFunction strategy   input
        = uncurry map $ bimap (flip compose) strategy input

    dfs :: Strategy
    dfs (SLDTree resolutions) = concatMap (mapFunction dfs) resolutions
    dfs Success = [empty]

    bfs :: Strategy
    bfs (SLDTree resolutions) = bfsConcat $ map (mapFunction bfs) resolutions
      where
          bfsConcat :: [[a]] -> [a]
          bfsConcat []
            = []
          bfsConcat listOfLists@(_:_)
            = [head | (head : _) <- listOfLists]
              ++ bfsConcat [tail | (_:tail) <- listOfLists]

