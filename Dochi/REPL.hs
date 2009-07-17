module Dochi.REPL (runREPL) where

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


prettystack = concat . intersperse " " . reverse . map prettyprint . stack

newline = putStrLn []

runLine :: ChiState -> [AST] -> IO ChiState
runLine st ast =

    case (envCompile (exports st) ast) of

      Left err -> do putStrLn $ "Compile Error: " ++ err
                     return st

      Right ic -> do st' <- runDochi st ic
                     when (not . null $ stack st') $ putStrLn $ "\ESC[31m<<\ESC[0m " ++ (prettystack st')
                     newline
                     return st'


completion :: ChiState -> String -> IO [String]
completion st str = return $ mapMaybe f $ M.keys (exports st)
    where f k = if (take (length str) k) == str then (Just k) else Nothing


runREPL :: ChiState -> IO ()
runREPL st = do setBasicWordBreakCharacters " "
                interactive st


interactive :: ChiState -> IO ()
interactive st = do

  setCompletionEntryFunction (Just $ completion st)
  line <- readline "\ESC[32m>>\ESC[0m "

  case line of
    Nothing -> putStrLn "Finished" >> return ()
    Just "" -> interactive st
    Just l  -> case (dochiParseLine "interactive" l) of

                 Right (ILine ast) -> do
                        addHistory l
                        r <- try $ runLine st ast
                        case r of
                          Left err -> do putStrLn $ show (err :: IOException)
                                         newline
                                         interactive st
                          Right st' -> interactive st'

                 Right (IDef name ast) -> do
                        addHistory l
                        case (envCompile (exports st) ast) of
                          Left err -> putStrLn $ "Compile Error: " ++ err
                          Right ic -> interactive $ defWord "user" name ic st

                 Right (IMod name) -> do
                        addHistory l
                        interactive st

                 Left err -> do
                        addHistory l
                        putStrLn $ "Error " ++ (show err)
                        interactive st
