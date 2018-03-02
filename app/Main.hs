module Main where
    import REPL

    -- |This function just prints the welcome message and starts the REPL
    main :: IO ()
    main = do
            putStrLn "Welcome to Simple Prolog!"
            putStrLn "Type \":help\" for help."
            readPrompt initState
