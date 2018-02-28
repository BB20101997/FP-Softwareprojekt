module Main where
    import Type
    import REPL

    main :: IO ()
    main = do
            putStrLn "Welcome to Simple Prolog!"
            putStrLn "Type \":help\" for help."
            readPrompt initState
