module Main where

import Parser
import Type

{- Todo 1. File interpreter
        2. Goal parser
        3. Antwortsubstitution
-}

main :: IO ()
main = (putStrLn "Welcome to Simple Prolog!" >>
 putStrLn "Type \":help\" for help.") >> (readPrompt False)

readPrompt :: Bool -> IO ()
readPrompt strategy = putStr "?- " >> getLine >>=
              \input -> (interpretPrompt strategy input)


interpretPrompt :: Bool -> String -> IO ()
interpretPrompt strategy input | ":help"                  ==input   = putStrLn helpText >> (readPrompt strategy)
                               | ":quit"                  ==input   = putStrLn "Goodbye"
                               | ":info"                  ==input   = putStrLn "No aviable predicates, please load file"
                               | ":set "++(drop 5 input)  ==input   = setSearch strategy (drop 5 input)
                               | ":load "++(drop 6 input) ==input   = loadFile strategy (drop 6 input)-- >>= \file -> readPromptWithFile file
                               | otherwise                          = putStrLn "Error" >> (readPrompt strategy) --Goal parser spot

readPromptWithFile :: Bool -> Prog -> IO ()
readPromptWithFile strategy programm = (putStr "?- ") >> getLine >>=
                                  \input -> (interpretPromptWithFile strategy programm input)

interpretPromptWithFile :: Bool -> Prog -> String -> IO ()
interpretPromptWithFile strategy programm input = interpretPrompt strategy input -- Vorläufig





setSearch :: Bool -> String -> IO ()
setSearch oldStrategy newStrategy | newStrategy == "dfs" = (putStrLn "Strategy set to depth-first") >> (readPrompt False)
                                  | newStrategy == "bfs" = (putStrLn "Strategy set to breadth-first") >> (readPrompt True)
                                  | otherwise            = (putStrLn "Error strategy stays unchanged") >> (readPrompt oldStrategy)

loadFile :: Bool -> String -> IO ()
loadFile strategy filePath =  ((putStrLn ("Loading file "++filePath)) >>
                         (parseFile filePath)::IO (Either String Prog)) >>=
                          \result -> (fileReadingResult strategy result)

fileReadingResult :: Bool -> Either String Prog -> IO ()
fileReadingResult strategy (Left error) = putStrLn ("Couldn't read file, the following error occurred: "++error) >>
                                     readPrompt strategy
fileReadingResult strategy (Right prog) = putStrLn ("File read ") >> (readPromptWithFile strategy prog)


helpText:: String
helpText = "Commands available from the prompt: \n"++
  " <goal> Solves/proves the specified goal. \n"++
  " :help Shows this help message. \n"++
  " :info Shows all available predicates. \n"++
  " :load <file> Loads the specified file. \n"++
  " :quit Exits the interactive environment. \n"++
  " :set <strat> Sets the specified search strategy"++
  " where <strat> is one of ' dfs ' or ' bfs ' ."