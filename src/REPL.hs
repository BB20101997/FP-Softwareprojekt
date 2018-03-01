module REPL where
    --Read Eval Print Loop
    import Data.List

    import Lib
    import Parser
    import SLD
    import Strategy
    import Substitution
    import System.IO

    {-
        Todo 3. Antwortsubstitution
    -}
    -- / The state at the start of the Interface
    initState :: State
    initState = (dfs, Prog [])

    -- / Asks the user for an input for the prolog-interface
    readPrompt :: State -> IO ()
    readPrompt state = do
                            putStr "?- "
                            hFlush stdout --make sure user knows we are waiting for him
                            getLine >>= \input -> interpretPrompt state input

    -- / Interprets the input from the user of the interface
    interpretPrompt :: Action
    interpretPrompt state@(strategy, program) input
                    | ""                       == input  = readPrompt state -- ignore empty input and ask again
                    | ":help"                  == input  = printHelp state input
                    | ":quit"                  == input  = exit      state input
                    | ":info"                  == input  = printInfo state input
                    | ":set " ++ drop 5 input  == input  = setSearch state (drop 5 input)
                    | ":load " ++ drop 6 input == input  = loadFile  state (drop 6 input)
                    | otherwise                          = parseGoalAndEvalGoal state input

    -- / If the input was an prolog expression it prints the evaluation of the goal
    -- Otherwise it prints an error message
    parseGoalAndEvalGoal :: Action
    parseGoalAndEvalGoal state@(strategy, program) input = case parseWithVars input of
                         Left error -> putStrLn error >> readPrompt state
                         Right (goal, vars) -> let
                                                   sldTree   = sld strategy program goal
                                                   solutions = strategy sldTree
                                               in do
                                                   --print sldTree
                                                   --print $ pretty sldTree
                                                   --print $ prettyWithVars vars sldTree
                                                   outputSolutions vars solutions
                                                   readPrompt state

    -- / Prints one solution of a goal
    outputSolutions :: [(VarIndex, String)] -> [Subst] -> IO()
    outputSolutions _    []          = putStrLn "No further Solutions!"
    outputSolutions vars (head:rest) = do
                                        putStr $ prettyWithVars vars head
                                        promptFurtherSolutions  vars rest

    -- / Asks the user if more solutions should be displayed
    promptFurtherSolutions :: [(VarIndex, String)] -> [Subst] -> IO()
    promptFurtherSolutions vars rest = do
                                        hFlush stdout --make sure user knows we are waiting for him
                                        x <- getLine
                                        case x of
                                            "," -> outputSolutions vars rest    --next result
                                            "." -> return ()                    --don't print further results
                                            _   -> do                           --invalid response, retry
                                                    putStr "Invalid Command '"
                                                    putStr x
                                                    putStr "', valid are ',' and '.'!"
                                                    promptFurtherSolutions vars rest

    -- / Sets the search strategy the program should use on sld trees either 'dfs' or 'bfs' are correct inputs
    setSearch :: Action
    setSearch (oldStrategy, program) newStrategy
              | newStrategy == "dfs" = putStrLn "Strategy set to depth-first"    >> readPrompt (dfs, program)
              | newStrategy == "bfs" = putStrLn "Strategy set to breadth-first"  >> readPrompt (bfs, program)
              | otherwise            = putStrLn "Error strategy stays unchanged" >> readPrompt (oldStrategy, program)

    -- / Loads a prolog file
    loadFile :: Action
    loadFile state filePath = do
                                putStrLn ("Loading file " ++ filePath)
                                parseResult <- parseFile filePath
                                newState <- fileReadingResult state parseResult
                                readPrompt newState

    -- / Prints the result of the prolog file loading, if an error occurred the old program is used
    fileReadingResult :: State -> Either String Prog -> IO State
    fileReadingResult state                  (Left error)
                      = putStrLn ("Couldn't read file, the following error occurred: " ++ error) >> return state
    fileReadingResult (strategy, oldProgram) (Right prog) = putStrLn "File read "                >> return (strategy, prog)

    -- / Closes the prolog interface
    exit :: Action
    exit _ _ = putStrLn "Goodbye"

    -- / Shows if a program is currently loaded, if it is it shows the aviable predicates
    printInfo :: Action
    printInfo state@(strategy,Prog [])      _ = putStrLn "No aviable predicates, please load file" >> readPrompt state
    printInfo state@(strategy,Prog program) _ = printPredicates(sort (nub (map showPredicates program))) >> readPrompt state

    -- / Prints the predicates of a program
    printPredicates :: [String] -> IO ()
    printPredicates predicates = putStr (foldr (++) "" predicates)

    -- / Returns a predicate with number of its arguments
    showPredicates :: Rule -> String
    showPredicates (Comb nameOfPredicate listOfArguments :- predicateBody ) = nameOfPredicate ++ "/" ++
                                                                               (show (length listOfArguments)) ++ "\n"
    showPredicates (Var _ :- _) = "" -- Impossible in prolog syntax

    -- / Shows the commands that can be used in the prolog interface
    printHelp :: Action
    printHelp state _ = putStrLn helpText >> readPrompt state

    -- / The Text with the aviable commands of the prolog interface
    helpText :: String
    helpText =  "Commands available from the prompt: \n" ++
                " <goal> Solves/proves the specified goal. \n" ++
                " :help Shows this help message. \n" ++
                " :info Shows all available predicates. \n" ++
                " :load <file> Loads the specified file. \n" ++
                " :quit Exits the interactive environment. \n" ++
                " :set <strat> Sets the specified search strategy" ++
                " where <strat> is one of ' dfs ' or ' bfs ' ."
