module Dochi.Interpreter where

import Data.Maybe (fromMaybe)
import Data.List (tails)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Error
import Data.Foldable (foldrM)

import Dochi.IMC
import Dochi.Parse (AST, ChiModuleAST, modName, modDefs, allExports)
import Dochi.Compile (envCompile)

-- mapping of words to code
type ChiModule = M.Map String (Chi ())
type Env = M.Map String ChiModule

-- global environment
data ChiState = ChiState
    { stack :: [Value]
    , vars :: [Value]
    , wordTrace :: [String]
    , env :: Env
    }

-- Chi Monad: interpreter code that manipulates the global state
type Chi a = StateT ChiState IO a


emptyState = ChiState [] [] [] M.empty


-- list of exports from modules

exports :: ChiState -> M.Map String String
exports st = M.foldWithKey (\name v m -> M.foldWithKey (\k _ m -> M.insert k name m) m v) M.empty (env st)


-- inject [ChiModuleAST] into ChiState
-- (allExports prog) placeholder for current module imports
-- e is currently union of allw ords in prog, and all words in state

injectAST :: [ChiModuleAST] -> ChiState -> Either String ChiState
injectAST prog st =

  let e = M.union (exports st) (allExports prog)

      modCompile :: ChiModuleAST -> ChiState -> Either String ChiState
      modCompile m st = foldrM (defCompile m) st (modDefs m)

      defCompile :: ChiModuleAST -> (String, [AST]) -> ChiState -> Either String ChiState
      defCompile m (name,ast) st = case (envCompile e ast) of
                                           Left err -> throwError $ "Compile Error in " ++ (modName m) ++ "."  ++ name ++  ": " ++ err
                                           Right ic -> return $ defWord (modName m) name ic st

  in foldrM modCompile st prog


injectLib :: String -> ChiModule -> ChiState -> ChiState
injectLib name m st = st { env = M.union (env st) (M.fromList [(name, m)]) }


-- dochi runtime error
prettyTrace tr = "\n\n\ESC[31mSTACK TRACE\ESC[0m:\n" ++ (concatMap (++"\n") tr)
chiError str = get >>= fail . (str++) . prettyTrace . wordTrace


popstack :: Chi Value
popstack = do
  st@ChiState{stack = a} <- get
  when (null a) $ chiError "Stack underflow."
  put $ st { stack = tail a }
  return $ head a

pushstack :: Value -> Chi ()
pushstack value = modify $ \st@ChiState{stack = a} -> st{ stack = (value:a) }

pushvar :: Value -> Chi ()
pushvar value = modify $ \st@ChiState{vars = a} -> st{ vars = (value:a) }

varpush :: String -> Chi ()
varpush name = do
  st@ChiState{stack = a, vars = b} <- get
  case a of
    [] -> chiError $ "Stack underflow. Expecting " ++ name ++ "."
    _ -> put $ st{ stack = tail a, vars = (head a:b) }

varpop :: Integer -> Chi ()
varpop count = modify $ \st@ChiState{vars = a} -> st { vars = tails a !! (fromInteger count) }

varindex :: Integer -> Chi ()
varindex n = do
  v <- gets vars
  pushstack $ v !! (fromInteger n)


pushTrace :: String -> Chi ()
pushTrace name = modify $ \st -> st {wordTrace = (name:wordTrace st)}

popTrace :: Chi ()
popTrace = modify $ \st -> st {wordTrace = (tail $ wordTrace st)}

callword :: String -> String -> Chi ()
callword m w = do
  e <- gets env
  case M.lookup m e of
    Just m' -> case M.lookup w m' of
                 Just f -> if m /= "core" then pushTrace w >> f >> popTrace else f
                 Nothing -> chiError $ "Unknown word: " ++ w ++ "."
    Nothing -> chiError $ "Unknown module: " ++ m ++ "."

makeclosure :: [Int] -> [IC] -> Chi ()
makeclosure refs code = do
  v <- gets vars
  pushstack $ VClosure (map (v !!) refs) code

fncall = do
  a <- popstack
  case a of
    VQuot code -> runCode code
    VClosure vals code -> do mapM_ pushvar vals
                             runCode code
    _ -> chiError $ "Cannot call value " ++ show a ++ "."

-- run IC code in Chi monad (IO with state)

runCode :: [IC] -> Chi ()
runCode [] = return ()

runCode (instr:code) = do

  case instr of
    PushValue v           -> pushstack v
    PopValue              -> popstack >> return ()
    VarPush name          -> varpush name
    EndScope count        -> varpop count
    VarIndex n            -> varindex n
    CallWord m w          -> callword m w
    FnCall                -> fncall
    MakeClosure refs code -> makeclosure refs code

  runCode code


-- Define a new word in ChiState

defWord :: String -> String -> [IC] -> ChiState -> ChiState
defWord m name code st = case M.lookup m (env st) of
                           Just m' -> st { env = M.insert m (M.insert name (runCode code) m') (env st) }
                           Nothing -> st { env = M.insert m (M.fromList [(name, (runCode code))]) (env st) }



-- run a word

runWord :: String -> String -> ChiState -> IO ChiState
runWord m name st = case M.lookup m (env st) of
                      Nothing -> error $ "Module not found: " ++ m
                      Just m' -> case M.lookup name m' of
                                   Nothing -> error $ "Word not found: " ++ name
                                   Just ic -> do (a, s) <- runStateT ic st
                                                 return s


-- StateT wrapper for runcode

runDochi :: ChiState -> [IC] -> IO ChiState
runDochi st code = do (a, s) <- runStateT (runCode code) st
                      return s
