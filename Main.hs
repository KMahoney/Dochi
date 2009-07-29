module Main where


import System.Environment
import System.Console.GetOpt
import qualified Data.Map as M

import Dochi.Interpreter (env)
import Dochi.Util (compileFiles, runFiles, initialState)
import qualified Dochi.REPL as R


data Options = Options { repl :: Bool
                       , showAST :: Bool
                       , showIC :: Bool
                       }

defaultOptions = Options { repl = False
                         , showAST = False
                         , showIC = False
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option ['i'] ["interactive"] (NoArg (\o -> o {repl=True})) "Interactive REPL"
          , Option [] ["ast"] (NoArg (\o -> o {showAST=True})) "Display Parse Output"
          , Option [] ["debug"] (NoArg (\o -> o {showIC=True})) "Display Intermediate Code"
          ]

getopts :: [String] -> IO (Options, [String])
getopts args = 
    case getOpt Permute options args of
      (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> error $ concat errs ++ usageInfo header options

header = "Usage: dochi [OPTION...] input"

withFiles files fn =
    case (length files) of
      0 -> error $ usageInfo header options
      _ -> fn files


main = do

  args <- getArgs
  (opts, files) <- getopts args

  case opts of

    Options {repl = True} -> do st <- compileFiles initialState files
                                R.runREPL R.Options { R.showIC = showIC opts, R.showAST = showAST opts, R.current = "user", R.using = [] } st

    _                     -> withFiles files runFiles

