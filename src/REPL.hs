module REPL where
    --Read Eval Print Loop
    import Type
    import Parser
    import Pretty
    import System.IO
    import Strategy
    import Substitution
    import SLD

    type State = (Strategy,Prog)
    type Action = State -> String -> IO ()

    {-
        Todo 3. Antwortsubstitution
    -}

    --TODO decide if we should set a default else handle undefined strategy where necessary
    initState::State
    initState = (undefined,Prog [])

    readPrompt :: State -> IO ()
    readPrompt state = do
                            putStr "?- "
                            hFlush stdout --flushing to prevent user staring at an empty Screen
                            getLine >>= \input -> interpretPrompt state input

    fileReadingResult :: State -> Either String Prog -> IO State
    fileReadingResult state                  (Left error) = putStrLn ("Couldn't read file, the following error occurred: "++error) >> return state
    fileReadingResult (strategy, oldProgram) (Right prog) = putStrLn "File read "                                                  >> return (strategy, prog)

    interpretPrompt :: Action
    interpretPrompt state@(strategy, program) input
                                    | ""                       == input  = readPrompt state -- ignore empty input and ask again
                                    | ":help"                  == input  = printHelp state input
                                    | ":quit"                  == input  = exit      state input
                                    | ":info"                  == input  = printInfo state input
                                    | ":set " ++ drop 5 input  == input  = setSearch state (drop 5 input)
                                    | ":load " ++ drop 6 input == input  = loadFile  state (drop 6 input)
                                    | otherwise                          = parseGoalAndEvalGoal state input

    --TODO if we decide to not set a default strategy this needs to be handled here
    parseGoalAndEvalGoal::Action
    parseGoalAndEvalGoal state@(strategy,program) input = case parseWithVars input of
                                                          Left error -> putStrLn error >> readPrompt state
                                                          Right (goal,vars) -> let
                                                                                    sldTree   = sld program goal
                                                                                    solutions = strategy sldTree
                                                                               in do
                                                                                    --print sldTree
                                                                                    --print $ pretty sldTree
                                                                                    --print $ prettyWithVars vars sldTree
                                                                                    outputSolutions vars solutions
                                                                                    readPrompt state

    outputSolutions::[(VarIndex,String)]->[Subst]->IO()
    outputSolutions _ [] = putStrLn "No further Solutions!"
    outputSolutions vars (head:rest) = do
                                        putStr $ prettyWithVars vars head
                                        promptFurtherSolutions  vars rest

    promptFurtherSolutions::[(VarIndex,String)]->[Subst]->IO()
    promptFurtherSolutions vars rest = do
                                        x <- getChar
                                        putStrLn ""
                                        case x of
                                            ',' -> outputSolutions vars rest    --next result
                                            '.' -> return ()                    --don't print further results
                                            _   -> do                           --invalid response, retry
                                                    putStr "Invalid Command '"
                                                    putChar x
                                                    putStr "', valid are ',' and '.'!"
                                                    promptFurtherSolutions vars rest

    setSearch :: Action
    setSearch (oldStrategy, program) newStrategy
                                        | newStrategy == "dfs" = putStrLn "Strategy set to depth-first"    >> readPrompt (dfs, program)
                                        | newStrategy == "bfs" = putStrLn "Strategy set to breadth-first"  >> readPrompt (bfs, program)
                                        | otherwise            = putStrLn "Error strategy stays unchanged" >> readPrompt (oldStrategy, program)

    loadFile :: Action
    loadFile state filePath = do
                                putStrLn ("Loading file "++filePath)
                                parseResult <- parseFile filePath
                                newState <- fileReadingResult state parseResult
                                readPrompt newState

    exit::Action
    exit _ _ = putStrLn "Goodbye"

    printInfo::Action --TODO advanced pretty print would need to have the variable names as part of the state
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
