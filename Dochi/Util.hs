module Util where

import System.IO
import Control.Monad (liftM)

import Parse (ChiModuleAST, dochiParseFile)
import Interpreter (ChiState, injectAST, runWord, emptyState)
import Core (coreState)


initialState = coreState emptyState

parseFile :: String -> IO [ChiModuleAST]
parseFile name = do
  content <- readFile name
  case (dochiParseFile name content) of
    Left err -> hPrint stderr err >> error "Parse Error"
    Right p -> return p


compileFiles :: ChiState -> [String] -> IO ChiState
compileFiles st files = do 
  prog <- liftM concat $ mapM parseFile files
  case (injectAST prog st) of
    Left err -> error err
    Right st' -> return st'


runFilesWithState :: ChiState -> [String] -> IO ()
runFilesWithState st files = do
  st' <- compileFiles st files
  runWord "main" "main" st'
  return ()

runFiles :: [String] -> IO ()
runFiles = runFilesWithState initialState
