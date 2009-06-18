module Interpreter where

import IC(IC(..), Value(..))

import Data.Maybe (fromMaybe)
import Data.List (tails)
import qualified Data.Map as M
import Control.Monad.State

data ChiState = ChiState
    { stack :: [Value]
    , vars :: [Value]
    , env :: M.Map String (Chi ())
    }

type Chi a = StateT ChiState IO a


chiError str = do st <- get
                  err $ str ++ 
                          "\nstack: " ++ (show $ stack st) ++
                          "\nvars: " ++ (show $ vars st)

    where err = liftIO . ioError . userError


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


callword :: String -> Chi ()
callword w = do
  e <- gets env
  case M.lookup w e of
    Just f -> f
    Nothing -> chiError $ "Unknown word: " ++ w ++ "."

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
    VTable t -> do v <- popstack
                   pushstack $ fromMaybe VNil $ M.lookup v t
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
    CallWord w            -> callword w
    FnCall                -> fncall
    MakeClosure refs code -> makeclosure refs code

  runCode code


-- Define a new word in ChiState

defWord :: String -> [IC] -> ChiState -> ChiState
defWord name code st@ChiState{env = e} = st { env = M.insert name (runCode code) e }


-- run a word
runWord :: String -> ChiState -> IO ChiState
runWord name  st = case M.lookup name (env st) of
                     Nothing -> error $ "Word not found: " ++ name
                     Just ic -> do (a, s) <- runStateT ic st
                                   return s



-- StateT wrapper for runcode

runDochi :: ChiState -> [IC] -> IO ChiState
runDochi st code = do (a, s) <- runStateT (runCode code) st
                      return s
