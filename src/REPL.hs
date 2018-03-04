{-|
    Read Eval Print Loop
-}
module REPL where
    import qualified Data.List as List
    import qualified System.IO as IO

    import qualified Parser
    import qualified SLD
    import qualified Rule
    import Strategy(dfs,bfs)
    import Lib  ( State, Action, VarIndex, Subst(..)
                , Prog(..), Term(..), Rule(..), Pretty(..)
                )

    -- |The state at the start of the Interface
    initState :: State
    initState = (dfs, Prog [])

    -- |Asks the user for an input for the prolog-interface
    readPrompt :: State -> IO ()
    readPrompt state = do
                            putStr "?- "
                            -- make sure user knows we are waiting for him
                            IO.hFlush IO.stdout
                            getLine >>= \input -> interpretPrompt state input

    {-|
        The map from the name of an action to it's function
        for the main prompt
    -}
    menuEntries::[(String,Action)]
    menuEntries =   [(":help",printHelp)
                    ,(":quit",exit)
                    ,(":info",printInfo)
                    ,(":set",setSearch)
                    ,(":load",loadFile)
                    ]

    -- |Interprets the input from the user of the interface
    interpretPrompt :: Action
    interpretPrompt state input
                    -- ignore empty input and ask again
                    | "" == input
                    = readPrompt state
                    -- try to find menu entry
                    | Just action <- lookup (head $ words input) menuEntries
                    = action state $ unwords $ tail $ words input
                    -- parse it as a goal and evaluate it
                    | otherwise
                    = parseGoalAndEvalGoal state input

    {-|
        If the input was an prolog expression
            then
                it evaluates the goal and prints the result
            else
                it prints an error message
    -}
    parseGoalAndEvalGoal :: Action
    parseGoalAndEvalGoal state@(strategy, program) input
        = case Parser.parseWithVars input of
            -- Print parser error message
            Left err           -> putStrLn err >> readPrompt state
            -- Parsing successful evaluate goal
            Right (goal, vars) ->
                let
                    sldTree   = SLD.sld strategy program goal
                    solutions = strategy sldTree
                in do
                    -- putStrLn $ prettyWithVars vars sldTree
                    outputSolutions vars solutions
                    readPrompt state

    -- |Prints one solution of a goal
    outputSolutions :: [(VarIndex, String)] -> [Subst] -> IO()
    outputSolutions _    []
        = putStrLn "No further Solutions!"
    outputSolutions vars (x:xs)
        = do
            putStr $ prettyWithVars vars $ filterOutput vars x
            promptFurtherSolutions  vars xs

    {-|
        Filters out all unnamed/anonymous Variable Substitutions
        from a Substitution
    -}
    filterOutput :: [(VarIndex,String)] -> Subst -> Subst
    filterOutput vars (Subst sub)
        = Subst [ s
                | s@(i, _) <- sub
                , i `elem`  [v | (v, name) <- vars, name /= "_"]
                ]

    -- |Asks the user if more solutions should be displayed
    promptFurtherSolutions :: [(VarIndex, String)] -> [Subst] -> IO()
    promptFurtherSolutions vars rest
        = do
            -- make sure user knows we are waiting for him
            IO.hFlush IO.stdout
            x <- getLine
            case x of
                -- next result
                "," -> outputSolutions vars rest
                -- don't print further results
                "." -> return ()
                -- invalid response, retry
                _   -> do
                        putStr "Invalid Command '"
                        putStr x
                        putStr "', valid are ',' and '.'!"
                        promptFurtherSolutions vars rest

    {-|
        Sets a new Search Strategy
        If the input is neither dfs nor bfs the current strategy will persist
    -}
    setSearch :: Action
    setSearch (old, program) newStrategy
        | newStrategy == "dfs"
        = putStrLn "Strategy set to depth-first"    >> readPrompt (dfs, program)
        | newStrategy == "bfs"
        = putStrLn "Strategy set to breadth-first"  >> readPrompt (bfs, program)
        | otherwise
        = putStrLn "Error strategy stays unchanged" >> readPrompt (old, program)

    -- |Loads a prolog file
    loadFile :: Action
    loadFile state filePath = do
                                putStrLn ("Loading file " ++ filePath)
                                parseResult <- Parser.parseFile filePath
                                newState <- fileReadingResult state parseResult
                                readPrompt newState
    {-|
        Prints the result of the prolog file loading,
        if an error occurred the old program is used
    -}
    fileReadingResult :: State -> Either String Prog -> IO State
    fileReadingResult state         (Left e)
        = do
            putStrLn ("Couldn't read file, the following error occurred:" ++ e)
            return state
    fileReadingResult (strategy, _) (Right prog)
        = do
            putStrLn "File read"
            return (strategy, prog)

    -- |Closes the prolog interface
    exit :: Action
    exit _ _ = putStrLn "Goodbye"

    -- |Shows all available predicates
    printInfo :: Action
    printInfo state@(_, Prog prog) _
        = do
            putStrLn "Buildin Predicates always show with Zero Arguments!"
            printPredicates(sn . map showPredicates $ prog ++ rules)
            readPrompt state
          where
            sn :: Ord a => [a] -> [a]
            sn    = List.sort.List.nub

            rules :: [Rule]
            rules = Rule.buildInToPrologRule Rule.predefinedRules

    -- |Prints the predicates of a program
    printPredicates :: [String] -> IO ()
    printPredicates predicates = putStr (concat predicates)

    -- |Returns a predicate with number of its arguments
    showPredicates :: Rule -> String
    showPredicates (Comb nameOfPredicate listOfArguments :- _)
        = nameOfPredicate ++ "/" ++ show (length listOfArguments) ++ "\n"
    -- |Impossible in prolog syntax
    showPredicates (Var _ :- _) = ""

    -- |Shows the commands that can be used in the prolog interface
    printHelp :: Action
    printHelp state _ = putStrLn helpText >> readPrompt state

    -- |The Text with the available commands of the prolog interface
    helpText :: String
    helpText =  "Commands available from the prompt: \n" ++
                " <goal> Solves/proves the specified goal. \n" ++
                " :help Shows this help message. \n" ++
                " :info Shows all available predicates. \n" ++
                " :load <file> Loads the specified file. \n" ++
                " :quit Exits the interactive environment. \n" ++
                " :set <strat> Sets the specified search strategy" ++
                " where <strat> is one of ' dfs ' or ' bfs ' ."
