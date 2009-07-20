module Dochi.REPL (runREPL, Options(..)) where

import System.Console.Editline.Readline

import Control.Exception
import Control.Monad (when)
import Data.List (concat, intersperse)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Dochi.Parse (AST, Interactive(..), dochiParseLine)
import Dochi.Compile (envCompile)
import Dochi.Interpreter (ChiState(..), defWord, runDochi, exports)
import Dochi.Core (prettyprint)



data Options = Options { showIC :: Bool
                       , showAST :: Bool
                       }


prettystack = concat . intersperse " " . reverse . map prettyprint . stack

newline = putStrLn []


debugAST opts ast = when (showAST opts) $ do putStrLn $ "AST: "
                                             mapM_ (putStrLn . ("    "++) . show) ast


debugIC opts ic = when (showIC opts) $ do putStrLn $ "IC: "
                                          mapM_ (putStrLn . ("    "++) . show) ic



runLine :: Options -> ChiState -> [AST] -> IO ChiState
runLine opts st ast = do

    case (envCompile (exports st) ast) of

      Left err -> do putStrLn $ "Compile Error: " ++ err
                     return st

      Right ic -> do debugIC opts ic
                     st' <- runDochi st ic
                     when (not . null $ stack st') $ putStrLn $ "\ESC[31m<<\ESC[0m " ++ (prettystack st')
                     newline
                     return st'


completion :: ChiState -> String -> IO [String]
completion st str = return $ mapMaybe f $ M.keys (exports st)
    where f k = if (take (length str) k) == str then (Just k) else Nothing


runREPL :: Options -> ChiState -> IO ()
runREPL opts st = do setBasicWordBreakCharacters " "
                     interactive opts st


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
                        debugAST opts ast
                        r <- try $ runLine opts st ast
                        case r of
                          Left err -> do putStrLn $ show (err :: Exception)
                                         newline
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
