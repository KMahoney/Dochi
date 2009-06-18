module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Console.Editline.Readline

import Prelude hiding (catch)
import Control.Exception (try, catch, IOException)
import Control.Monad (when, liftM)
import Data.List (concat)


import Parse
import IC (compileScoped)
import Interpreter (runDochi, defWord, runWord, stack, ChiState)
import Core (coreState)


-- filename utils

basename = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse
addslash dir = if ((head $ reverse dir) == '/') then dir else (dir ++ "/")



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
          , Option [] ["debug"] (NoArg (\o -> o {debug=True})) "Debug info"
          , Option ['v'] ["verbose"] (NoArg (\o -> o {verbose=True})) "Verbose mode"
          , Option ['i'] ["interactive"] (NoArg (\o -> o {repl=True})) "Interactive REPL" ]

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
    then compileFiles files >>= interactive opts
    else case (length files) of
           0 -> error $ usageInfo header options
           _ -> compileFiles files >>= runWord "main" >> return ()


-- interactive

runLine :: Options -> ChiState -> [AST] -> IO ChiState
runLine opts st ast =
  case (compileScoped [] ast) of
    Left err -> do putStrLn $ "Compile Error: " ++ err
                   return st
    Right ic -> do when (debug opts) $ putStrLn $ show ic
                   st' <- runDochi st ic
                   putStrLn []
                   return st'

interactive :: Options -> ChiState -> IO ()
interactive opts st = do
  line <- readline ">> "
  case line of
    Nothing -> putStrLn "Finished" >> return ()
    Just "" -> interactive opts st
    Just l  -> case (dochiParse "interactive" l) of

                 Right (ILine ast) -> do
                        addHistory l
                        r <- try $ runLine opts st ast
                        case r of
                          Left err -> do putStrLn $ show (err :: IOException)
                                         putStrLn ""
                                         interactive opts st
                          Right st' -> interactive opts st'

                 Right (IDef name ast) -> do
                        addHistory l
                        case (compileScoped [] ast) of
                          Left err -> putStrLn $ "Compile Error: " ++ err
                          Right ic -> interactive opts $ defWord name ic st

                 Left err  -> do
                        putStrLn $ "Error " ++ (show err)
                        interactive opts st


-- run file

parseFile :: String -> IO Prog
parseFile name = do
  content <- readFile name
  case (dochiParseFile name content) of
    Left err -> hPrint stderr err >> error "Parse Error"
    Right p -> return p


compileFiles :: [String] -> IO ChiState
compileFiles files = do
  prog <- liftM concat $ mapM parseFile files
  return $ foldr f coreState prog

    where f (name, ast) st = case (compileScoped [] ast) of
                               Left err -> error $ "Compile Error: " ++ err
                               Right ic -> defWord name ic st
