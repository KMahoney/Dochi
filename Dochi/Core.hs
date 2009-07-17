module Dochi.Core where

import Dochi.IMC
import Dochi.Interpreter

import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad.State
import Random

prettyprint v = 
    case v of
      VString a -> a
      VInteger a -> show a
      VWord a -> "/" ++ a
      VKeyword a -> ":" ++ a
      VBool False -> "f"
      VBool True -> "t"
      VQuot _ -> "[QUOT]"
      VClosure vals _ -> "[closure over " ++ (intercalate " " $ map prettyprint vals) ++ "]"
      VCons h t -> "L{" ++ pplist h t
      VTable t -> "T{" ++ (intercalate " " $ map pptable $ M.toList t) ++ "}"

    where pplist h (VBool False) = prettyprint h ++ "}"
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

toString = do
  v <- popstack
  pushstack $ VString $ prettyprint v

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
  pushstack $ VBool (fn b a)

equality = do
  a <- popstack
  b <- popstack
  pushstack $ VBool (b == a)

ifstmt = do       
  f <- popstack
  t <- popstack
  c <- popstack
  pushstack $ if (c /= (VBool False)) then t else f
  fncall

clearstack = modify $ \st -> st { stack = [] }
  

-- lists

listCons = do
  h <- popstack
  t <- popstack
  pushstack $ VCons h t

checkedCons = do
  v <- popstack
  case v of
    VCons h t -> return (h, t)
    _ -> chiError "Expecting list"

listHead = do
  (h, _) <- checkedCons
  pushstack $ h

listTail = do
  (_, t) <- checkedCons
  pushstack $ t

listLength = let calc n (VCons _ b) = calc (n + 1) b
                 calc n _ = n
             in do l <- popstack
                   pushstack $ VInteger $ calc 0 l

listNth = let calc 0 (VCons x _) = return x
              calc n (VCons _ b) = calc (n - 1) b
              calc _ _ = chiError "Expecting list"
          in do i <- checkedInteger
                l <- popstack
                v <- calc i l
                pushstack v


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

inserttable = do
  k <- popstack
  v <- popstack
  t <- checkedTable
  pushstack $ VTable $ M.insert k v t

gettable = do
  k <- popstack
  t <- checkedTable
  pushstack $ fromMaybe (VBool False) $ M.lookup k t



-- misc

rand_gen = do
  b <- checkedInteger
  a <- checkedInteger
  n <- liftIO $ getStdRandom (randomR (a,b))
  pushstack $ VInteger n


corelib = M.fromList
          [ (".", doprettyprint)
          , ("write", writestr)
          , ("->string", toString)
          , ("clear", clearstack)

          , (".s", printstack)
          , (".e", printenv)
          , (".v", printvars)

          , ("<<", inserttable)
          , (">>", gettable)

          , ("if", ifstmt)

          , (";",      listCons)
          , ("head",   listHead)
          , ("tail",   listTail)
          , ("length", listLength)
          , ("nth",    listNth)

          , ("+", bin_math (+))
          , ("-", bin_math (-))
          , ("*", bin_math (*))
          , ("/", bin_math (div))

          , ("<", bin_bool_math (<))
          , (">", bin_bool_math (>))
          , ("<=", bin_bool_math (<=))
          , (">=", bin_bool_math (>=))

          , ("=", equality)

          , ("rand-range", rand_gen)
          ]

coreState = injectLib "core" corelib
