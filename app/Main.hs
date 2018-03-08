module Main where
    import qualified REPL(readPrompt, initState)

    -- |This function just prints the welcome message and starts the REPL
    main :: IO ()
    main = do
            putStrLn "Welcome to Simple Prolog!"
            putStrLn "Type \":help\" for help."
            putStrLn "Default search is depth-first."
            putStrLn ("Default Parser is OurParser that"++
             " adds parsing for tuple and ';'.")
            REPL.readPrompt REPL.initState
