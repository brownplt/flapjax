#!/usr/bin/env runhaskell
import Distribution.Simple
import qualified Data.List as L
import System.Directory
import qualified Data.Char as Ch
import System.Process (runCommand,waitForProcess)

isHaskellFile file = ".lhs" `L.isSuffixOf` file || ".hs" `L.isSuffixOf` file

moduleName file = L.takeWhile  (/= '.') file

isRequested :: [String] -> String -> Bool
isRequested requestedTests test = 
  (map Ch.toLower test) `elem` (map (map Ch.toLower) requestedTests)

testMain args _ _ _ = do
  files <- getDirectoryContents "tests"
  let testFiles = filter isHaskellFile files
  let testModules = if null args
                      then map moduleName testFiles
                      else filter (isRequested args) (map moduleName testFiles)
  let testFuncs = map (++ ".main") testModules
  let testExpr = "sequence [ " ++ concat (L.intersperse "," testFuncs) ++ 
                 " ] >>= \\cases -> runTestTT (TestList cases)"
  let moduleLine = concat (L.intersperse " " testModules)
  let cmd = "cd tests && ghc  -XNoMonomorphismRestriction -fglasgow-exts " ++
            "-package HUnit -package parsec-2.1.0.1 -i:../src -e \"" ++ 
            testExpr ++ " >> return ()\" " ++ moduleLine
  putStrLn "Testing command is:"
  putStrLn cmd
  putStrLn "\nLoading tests..."
  handle <- runCommand cmd
  waitForProcess handle
  putStrLn "Testing complete.  Errors reported above (if any)."


main = defaultMainWithHooks (simpleUserHooks { runTests = testMain })
