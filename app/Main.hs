module Main where
    import Type
    import Strategy
    import REPL


    main :: IO ()
    main = do
            putStrLn "Welcome to Simple Prolog!"
            putStrLn "Type \":help\" for help."
            readPromptWithFile initState
