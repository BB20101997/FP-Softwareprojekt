module REPL where
    --Read Eval Print Loop
    import Type
    import Parser
    import System.IO
    import Strategy

    type State = (Strategy,Prog)
    type Action = State -> String -> IO ()

    {- Todo 1. File interpreter
            2. Goal parser
            3. Antwortsubstitution
    -}
    initState::State
    initState = (undefined,Prog [])

    readPrompt :: State -> IO ()
    readPrompt state = do
                            putStr "?- "
                            hFlush stdout --flushing to prevent user staring at an empty Screen
                            getLine >>= \input -> interpretPrompt state input

    fileReadingResult state                  (Left error) = putStrLn ("Couldn't read file, the following error occurred: "++error) >> return state
    fileReadingResult (strategy, oldProgram) (Right prog) = putStrLn "File read "                                                  >> return (strategy, prog)

    interpretPrompt :: Action
    interpretPrompt state@(strategy, program) input
                                    | ":help"                  == input  = printHelp state input
                                    | ":quit"                  == input  = exit      state input
                                    | ":info"                  == input  = printInfo state input
                                    | ":set " ++ drop 5 input  == input  = setSearch state (drop 5 input)
                                    | ":load " ++ drop 6 input == input  = loadFile  state (drop 6 input)-- >>= \file -> readPromptWithFile file
                                    | otherwise                          = putStrLn "Error" >> readPrompt state--Goal parser spot

    setSearch :: Action
    setSearch (oldStrategy, program) newStrategy
                                        | newStrategy == "dfs" = putStrLn "Strategy set to depth-first"    >> readPrompt (dfs, program)
                                        | newStrategy == "bfs" = putStrLn "Strategy set to breadth-first"  >> readPrompt (bfs, program)
                                        | otherwise            = putStrLn "Error strategy stays unchanged" >> readPrompt (oldStrategy, program)

    fileReadingResult :: State -> Either String Prog -> IO State
    loadFile :: Action
    loadFile state filePath = do
                                putStrLn ("Loading file "++filePath)
                                parseResult <- parseFile filePath::IO (Either String Prog)
                                newState <- fileReadingResult state parseResult
                                readPrompt newState

    exit::Action
    exit _ _ = putStrLn "Goodbye"

    printInfo::Action
    printInfo state _ = putStrLn "No aviable predicates, please load file"

    printHelp::Action
    printHelp state _ = putStrLn helpText >> readPrompt state

    helpText:: String
    helpText =  "Commands available from the prompt: \n"++
                " <goal> Solves/proves the specified goal. \n"++
                " :help Shows this help message. \n"++
                " :info Shows all available predicates. \n"++
                " :load <file> Loads the specified file. \n"++
                " :quit Exits the interactive environment. \n"++
                " :set <strat> Sets the specified search strategy"++
                " where <strat> is one of ' dfs ' or ' bfs ' ."
