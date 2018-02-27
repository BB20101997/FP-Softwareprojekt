module Strategy where
    import Type
    import SLD
    import Substitution

    type Strategy = SLDTree -> [Subst]

    {-
        //TODO define dfs and bfs Strategy
    -}
    dfs::Strategy
    dfs = undefined

    bfs::Strategy
    bfs = undefined

