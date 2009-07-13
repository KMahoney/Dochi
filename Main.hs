module Main where


import System.IO
import System.Environment
import System.Console.GetOpt
import System.Console.Editline.Readline

import Prelude hiding (catch)
import Control.Exception (try, catch, IOException)
import Control.Monad (when, liftM)
import Data.List (concat, intersperse)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)



import Dochi.Parse (AST(..), Interactive(..), ChiModuleAST, dochiParseLine, dochiParseFile)
import Dochi.Compile (envCompile)
import Dochi.Interpreter (ChiState(..), injectAST, defWord, runDochi, exports, runWord, emptyState)
import Dochi.Core (coreState, prettyprint)
import Dochi.Util (compileFiles, runFiles, initialState)


-- options

data Options = Options { make :: Bool
                       , debug :: Bool
                       , verbose :: Bool
                       , repl :: Bool }


defaultOptions = Options { make = False
                         , debug = False
                         , verbose = False
                         , repl = False }

options :: [OptDescr (Options -> Options)]
options = [ Option ['c'] ["compile"] (NoArg (\o -> o {make=True})) "Compile input to C"
          , Option ['d'] ["debug"] (NoArg (\o -> o {debug=True})) "Debug info"
          , Option ['v'] ["verbose"] (NoArg (\o -> o {verbose=True})) "Verbose mode"
          , Option ['i'] ["interactive"] (NoArg (\o -> o {repl=True})) "Interactive REPL"
          ]

getopts :: [String] -> IO (Options, [String])
getopts args = 
    case getOpt Permute options args of
      (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> error $ concat errs ++ usageInfo header options

header = "Usage: dochi [OPTION...] input"


-- main entry

main = do
  args <- getArgs
  (opts, files) <- getopts args

  if repl opts
    then setBasicWordBreakCharacters " " >> compileFiles initialState files >>= interactive opts
    else case (length files) of
           0 -> error $ usageInfo header options
           _ -> runFiles files


-- interactive

debugAST opts ast = when (debug opts) $ do putStrLn $ "AST: "
                                           mapM_ (putStrLn . ("    "++) . show) ast

debugIC opts ic = when (debug opts) $ do putStrLn $ "IC: "
                                         mapM_ (putStrLn . ("    "++) . show) ic


runLine :: Options -> ChiState -> [AST] -> IO ChiState
runLine opts st ast = do
  debugAST opts ast
  case (envCompile (exports st) ast) of
    Left err -> do putStrLn $ "Compile Error: " ++ err
                   return st

    Right ic -> do debugIC opts ic
                   st' <- runDochi st ic
                   when (not . null $ stack st') $ putStrLn $ "\ESC[31m<<\ESC[0m " ++ (concat $ intersperse " " $ reverse $ map prettyprint (stack st'))
                   putStrLn []
                   return st'

completion :: ChiState -> String -> IO [String]
completion st str = return $ mapMaybe f $ M.keys (exports st)
    where f k = if (take (length str) k) == str then (Just k) else Nothing

interactive :: Options -> ChiState -> IO ()
interactive opts st = do
  setCompletionEntryFunction (Just $ completion st)
  line <- readline "\ESC[32m>>\ESC[0m "
  case line of
    Nothing -> putStrLn "Finished" >> return ()
    Just "" -> interactive opts st
    Just l  -> case (dochiParseLine "interactive" l) of

                 Right (ILine ast) -> do
                        addHistory l
                        r <- try $ runLine opts st ast
                        case r of
                          Left err -> do putStrLn $ show (err :: IOException)
                                         putStrLn []
                                         interactive opts st
                          Right st' -> interactive opts st'

                 Right (IDef name ast) -> do
                        addHistory l
                        debugAST opts ast
                        case (envCompile (exports st) ast) of
                          Left err -> putStrLn $ "Compile Error: " ++ err
                          Right ic -> do debugIC opts ic
                                         interactive opts $ defWord "user" name ic st

                 Right (IMod name) -> do
                        addHistory l
                        interactive opts st

                 Left err -> do
                        addHistory l
                        putStrLn $ "Error " ++ (show err)
                        interactive opts st