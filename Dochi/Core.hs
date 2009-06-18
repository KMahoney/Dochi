module Core (coreState) where

import IC(IC(..), Value(..))
import Interpreter

import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad.State

prettyprint v = 
    case v of
      VString a -> a
      VInteger a -> show a
      VWord a -> "/" ++ a
      VKeyword a -> ":" ++ a
      VTrue -> "true"
      VQuot _ -> "[QUOT]"
      VClosure vals _ -> "[closure over " ++ (intercalate " " $ map prettyprint vals) ++ "]"
      VNil -> "nil"
      VCons h t -> "L{" ++ pplist h t
      VTable t -> "T{" ++ (intercalate " " $ map pptable $ M.toList t) ++ "}"

    where pplist h VNil = prettyprint h ++ "}"
          pplist h (VCons h2 t2) = prettyprint h ++ " " ++ pplist h2 t2
          pplist h t = prettyprint h ++ " . " ++ prettyprint t ++ "}"
          pptable (k,v) = (prettyprint k) ++ " " ++ (prettyprint v)

doprettyprint = popstack >>= (liftIO . putStrLn . prettyprint)

checkedString = do
  v <- popstack
  case v of
    VString s -> return s
    _ -> chiError "Expecting String"

writestr = checkedString >>= (liftIO . putStr)

checkedInteger = do
  v <- popstack
  case v of
    VInteger i -> return i
    _ -> chiError "Expecting Integer"

bin_math fn = do
  a <- checkedInteger
  b <- checkedInteger
  pushstack $ VInteger $ fn b a

bin_bool_math fn = do
  a <- checkedInteger
  b <- checkedInteger
  pushstack $ if (fn b a) then VTrue else VNil

equality = do
  a <- popstack
  b <- popstack
  pushstack $ if (b == a) then VTrue else VNil

ifstmt = do       
  f <- popstack
  t <- popstack
  c <- popstack
  pushstack $ if (c /= VNil) then t else f
  fncall

clearstack = modify $ \st -> st { stack = [] }
  
vcons = do
  h <- popstack
  t <- popstack
  pushstack $ VCons h t

-- lists

checkedCons = do
  v <- popstack
  case v of
    VCons h t -> return (h, t)
    _ -> chiError "Expecting list"

makelist = pushstack VNil

vhead = do
  (h, _) <- checkedCons
  pushstack $ h

vtail = do
  (_, t) <- checkedCons
  pushstack $ t


-- interpreter state

printstack = do
  s <- gets stack
  let pp = concat $ intersperse " " $ reverse $ map prettyprint s
  liftIO $ putStrLn $ "{" ++ pp ++ "}"

printenv = do
  s <- gets env
  let pp = (concat . intersperse " " . M.keys) s
  liftIO $ putStrLn $ "{" ++ pp ++ "}"

printvars = do
  s <- gets vars
  let pp = concat $ intersperse " " $ reverse $ map prettyprint s
  liftIO $ putStrLn $ "{" ++ pp ++ "}"


-- tables

checkedTable = do
  v <- popstack
  case v of
    VTable t -> return t
    _ -> chiError "Expecting table"

maketable = pushstack $ VTable $ M.empty

inserttable = do
  k <- popstack
  v <- popstack
  t <- checkedTable
  pushstack $ VTable $ M.insert k v t

gettable = do
  k <- popstack
  t <- checkedTable
  pushstack $ fromMaybe VNil $ M.lookup k t



corelib = M.fromList
          [ (".", doprettyprint)
          , ("write", writestr)
          , ("clear", clearstack)
          , (".s", printstack)
          , (".e", printenv)
          , (".v", printvars)

          , ("<table>", maketable)
          , ("<<", inserttable)
          , (">>", gettable)


          , ("if", ifstmt)

          , ("<list>", makelist)
          , (";",    vcons)
          , ("head", vhead)
          , ("tail", vtail)

          , ("+", bin_math (+))
          , ("-", bin_math (-))
          , ("*", bin_math (*))
          , ("/", bin_math (div))

          , ("<", bin_bool_math (<))
          , (">", bin_bool_math (>))
          , ("<=", bin_bool_math (<=))
          , (">=", bin_bool_math (>=))

          , ("=", equality)
          ]

coreState = ChiState [] [] corelib
