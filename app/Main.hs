module Main where
    import qualified REPL(readPrompt, initState)

    -- |This function just prints the welcome message and starts the REPL
    main :: IO ()
    main = do
            putStrLn "Welcome to Simple Prolog!"
            putStrLn "Default search is depth-first."
            putStrLn ("Default parser is OurParser"++
                      " that adds parsing for Tupel and ';'.")
            putStrLn "Type \":help\" for help."
            REPL.readPrompt REPL.initState
