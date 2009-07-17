module Main where


import System.Environment
import System.Console.GetOpt

import Dochi.Util (compileFiles, runFiles, initialState)
import Dochi.REPL (runREPL)


data Options = Options { repl :: Bool }

defaultOptions = Options { repl = False }

options :: [OptDescr (Options -> Options)]
options = [ Option ['i'] ["interactive"] (NoArg (\o -> o {repl=True})) "Interactive REPL" ]

getopts :: [String] -> IO (Options, [String])
getopts args = 
    case getOpt Permute options args of
      (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> error $ concat errs ++ usageInfo header options

header = "Usage: dochi [OPTION...] input"


main = do

  args <- getArgs
  (opts, files) <- getopts args

  if repl opts
    then compileFiles initialState files >>= runREPL 
    else case (length files) of
           0 -> error $ usageInfo header options
           _ -> runFiles files

