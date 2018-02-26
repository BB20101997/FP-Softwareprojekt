import qualified SubstTest
main :: IO ()
main = do
            putStrLn "Running Substitution Test:"
            SubstTest.runTests
            return ()
