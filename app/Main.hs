module Main where
    import REPL

    main :: IO ()
    main = do
            putStrLn "Welcome to Simple Prolog!"
            putStrLn "Type \":help\" for help."
            readPrompt initState
