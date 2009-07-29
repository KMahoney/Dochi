module Dochi.Util where

import System.IO
import Control.Monad (liftM)
import qualified System.IO.Error as IOE

import Data.Either

import Dochi.Parse (ChiModuleAST, dochiParseFile)
import Dochi.Interpreter (ChiState, injectAST, runWord, emptyState)
import Dochi.Core (coreState)


initialState = coreState emptyState

parseFile :: String -> IO [ChiModuleAST]
parseFile name = readFile name >>= either parseError return . dochiParseFile name
    where parseError err = hPrint stderr err >> error "Parse Error"


compileFiles :: ChiState -> [String] -> IO ChiState
compileFiles st files = do 
  prog <- liftM concat $ mapM parseFile files
  either error return (injectAST prog st)

runFilesWithState :: ChiState -> [String] -> IO ()
runFilesWithState st files = compileFiles st files >>= runWord "main" "main" >> return ()

runFiles :: [String] -> IO ()
runFiles = runFilesWithState initialState

handleError :: IO () -> IO ()
handleError = flip catch showError
    where showError err = hPutStrLn stderr $ if IOE.isUserError err
                                               then IOE.ioeGetErrorString err
                                               else show err
