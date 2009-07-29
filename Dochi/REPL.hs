module Dochi.REPL (runREPL, Options(..)) where

import System.Console.Editline.Readline


import Control.Monad (when)
import Data.List (concat, intersperse)
import Data.Maybe (mapMaybe)
import qualified System.IO.Error as IOE
import qualified Data.Map as M

import Dochi.Parse (AST, Interactive(..), dochiParseLine)
import Dochi.Compile (envCompile)
import Dochi.Interpreter (ChiState(..), defWord, runDochi, environment)
import Dochi.Core (prettyprint)


data Options = Options { showIC :: Bool
                       , showAST :: Bool
                       , current :: String
                       , using :: [String]
                       }


prettystack = concat . intersperse " " . reverse . map prettyprint . stack

newline = putStrLn []


debugAST opts ast = when (showAST opts) $ do putStrLn $ "AST: "
                                             mapM_ (putStrLn . ("    "++) . show) ast


debugIC opts ic = when (showIC opts) $ do putStrLn $ "IC: "
                                          mapM_ (putStrLn . ("    "++) . show) ic



runLine :: Options -> ChiState -> [AST] -> IO ChiState
runLine opts st ast = do

    case (envCompile (environment st) (current opts:using opts) ast) of

      Left err -> do putStrLn $ "Compile Error: " ++ err
                     return st

      Right ic -> do debugIC opts ic
                     st' <- runDochi st ic
                     when (not . null $ stack st') $ putStrLn $ "\ESC[31mstack>\ESC[0m " ++ (prettystack st')
                     newline
                     return st'


completion :: Options -> ChiState -> String -> IO [String]
completion opts st str = return $ mapMaybe f $ filtered ++ all
    where f k = if (take (length str) k) == str then (Just k) else Nothing
          filtered = concatMap snd $ filter (flip elem modules . fst) $ environment st
          all = concatMap (\(m,w) -> map ((m ++ ".") ++) w) $ environment st
          modules = ("core":current opts:using opts)


runREPL :: Options -> ChiState -> IO ()
runREPL opts st = do setBasicWordBreakCharacters " "
                     interactive opts st


interactive :: Options -> ChiState -> IO ()
interactive opts st = do

  setCompletionEntryFunction (Just $ completion opts st)
  line <- readline $ (current opts) ++ "> "

  case line of
    Nothing -> putStrLn "Finished" >> return ()
    Just "" -> interactive opts st
    Just l  -> case (dochiParseLine "interactive" l) of

                 Right (ILine ast) -> do
                        addHistory l
                        debugAST opts ast
                        r <- IOE.try $ runLine opts st ast
                        case r of
                          Left err -> do if (IOE.isUserError err)
                                           then putStrLn $ IOE.ioeGetErrorString err
                                           else putStrLn $ show err
                                         newline
                                         interactive opts st
                          Right st' -> interactive opts st'

                 Right (IDef name ast) -> do
                        addHistory l
                        debugAST opts ast
                        case (envCompile (environment st) (current opts:using opts) ast) of
                          Left err -> putStrLn $ "Compile Error: " ++ err
                          Right ic -> do debugIC opts ic
                                         interactive opts $ defWord (current opts) name ic st

                 Right (IMod name) -> do
                        addHistory l
                        interactive opts { current = name } st

                 Left err -> do
                        addHistory l
                        putStrLn $ "Error " ++ (show err)
                        interactive opts st
