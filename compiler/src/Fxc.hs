-- Command-line based Flapjax compiler.  Run without any options for usage
-- information.
module Main where

import System.IO
import System.Console.GetOpt
import System.Environment hiding (withArgs)
import System.Directory
import Data.List as List
import WebBits.Common
import WebBits.Html.Html ()
import Text.PrettyPrint.HughesPJ
import Text.ParserCombinators.Parsec(ParseError,parseFromFile)
import Flapjax.HtmlEmbedding()
import Flapjax.Parser(parseScript) -- for standalone mode
import WebBits.Html.PermissiveParser(parseHtmlFromFile)
import Flapjax.Compiler(compilePage,compileStandalone,defaults,CompilerOpts(..))
import Computation

data CmdLineOpts
  = Flapjax String
  | Output String
  | Standalone
  | Loader String
  | WebMode
  deriving (Eq,Ord) -- Ord lets us avoid permutations.
  
isFlapjaxPath (Flapjax _) = True
isFlapjaxPath _           = False

isWebMode WebMode = True
isWebMode _       = False

isOutputPath (Output _) = True
isOutputPath _           = False

options:: [OptDescr CmdLineOpts]
options =
 [Option ['f'] ["flapjax-path"] (ReqArg Flapjax "URL") "url of flapjax.js",
  Option ['o'] ["output"]       (ReqArg Output "FILE") "output path",
  Option []    ["web-mode"]     (NoArg WebMode)        "web-compiler mode",
  Option []    ["standalone"]   (NoArg Standalone)     "standalone mode",
  Option []    ["loader"]       (ReqArg Loader "NAME") "loader name (standalone mode)"]

-- |Attempts to get the default Flapjax path from the environment, if it is
-- defined.
getFlapjaxPath args =
  case List.find isFlapjaxPath args of
    (Just (Flapjax path)) -> return path
    Nothing               -> do env <- getEnvironment
                                case lookup "FLAPJAXPATH" env of
                                  (Just path) -> return path
                                  Nothing     -> return "flapjax.js"

-- |Gets the output file, if specified.
getOutputPath args =
  case List.find isOutputPath args of
    (Just (Output path)) -> return (Just path)
    Nothing              -> return Nothing

-- |Indicates whether the compiler is being run in web-mode.
getWebMode args =
  case List.find isWebMode args of
    (Just WebMode) -> return True
    Nothing        -> return False

-- |Indicates whether the compiler is being run in standalone mode.  In 
--  standalone mode, the compiler expects only Javascript.
getStandalone args =
  let pred Standalone = True
      pred _          = False
    in case List.find pred args of
         (Just Standalone) -> return True
         Nothing           -> return False

-- |Returns the name of the loader, if specified.
getLoaderName args =
  let pred (Loader _) = True
      pred _          = False
    in case List.find pred args of
         (Just (Loader s)) -> return (Just s)
         Nothing           -> return Nothing
      
  
    
-- |Indicates whether the argument list is well-formed.
validateArgs args = v (List.sort args) where
  v ([Output o, Standalone, Loader l]) = True
  v ([Standalone, Loader l]) = True
  v ([Output o, Standalone]) = True
  v ([Standalone]) = True
  v ([Flapjax fj, Output o, WebMode]) = True
  v ([Flapjax fj, Output o]) = True
  v ([Output o, WebMode]) = True
  v ([Flapjax fj, WebMode]) = True
  v ([WebMode]) = True
  v ([Output o]) = True
  v ([Flapjax fj]) = True
  v ([]) = True
  v _ = False

-- |Validates the arguments and calls the specified continuation.  If the
-- arguments are invalid, displays a usage message and terminates.
withArgs args cont =
  case validateArgs args of
    False -> showUsage []
    True  -> do flapjaxPath  <- getFlapjaxPath args
                maybeOutFile <- getOutputPath args
                isWebMode    <- getWebMode args
                isStandalone <- getStandalone args
                loader       <- getLoaderName args
                cont (flapjaxPath,maybeOutFile,isWebMode,isStandalone,loader)

-- |This is what is head of the generated HTML when there are errors or warnings
-- (that are reported as errors) in web-compiler mode.
webHeader =
  "<html>\n<head><title>Flapjax Compiler</title></head><\nbody>\n"

webFooter = "</body></html>\n"

-- |Displays usage information and argument errors.
showUsage errors = do
  putStr "Flapjax Compiler (fxc) 2009-03-19\n"
  putStr (concat errors)
  putStr (usageInfo "Usage: fxc [OPTION ...] file" options)

output Nothing html = putStr (render $ pp html)
output (Just outFile) html = writeFile outFile (render $ pp html)

compile inFile (flapjaxPath,maybeOutFile,webMode,False,loader) = do -- page mode
  htmlOrError <- parseHtmlFromFile inFile
  case htmlOrError of
    (Left error) -> putStr $ "Parse error:\n" ++ show error
    (Right (html,warnings)) -> do 
      result <- runComputation $
        compilePage (defaults { flapjaxPath = flapjaxPath }) html
      case result of
        (Success ws v) -> output maybeOutFile v
        (Failure ws e) -> putStr "errors"
compile inFile (flapjaxPath,maybeOutFile,webmode,True,loader) = do -- standalone
  fxOrError <- parseFromFile parseScript inFile
  case fxOrError of
    (Left error) -> putStr $ "Parse error:\n" ++ show error
    (Right fx) -> do 
      result <- do runComputation (compileStandalone defaults loader fx)
      case result of
        (Success ws v) -> output maybeOutFile v
        (Failure ws e) -> putStr "errors"

main = do
  argv <- getArgs
  case getOpt Permute options argv of
     (args,[file],[]) -> withArgs args (compile file)
     (_,_,errors)      -> showUsage errors
