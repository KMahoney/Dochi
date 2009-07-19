module Dochi.Core where

import Dochi.IMC
import Dochi.Interpreter

import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad.State
import Random

prettylist li h (VCons h2 t2) = prettylist (h:li) h2 t2
prettylist li h (VBool False) = "L{" ++ (intercalate " " $ map prettyprint $ reverse $ h:li) ++ "}"
prettylist li h t = "C{" ++ (intercalate " " $ map prettyprint $ reverse $ t:h:li) ++ "}"

prettyprint v = 
    case v of
      VString a -> "\"" ++ a ++ "\""
      VChar a -> "Ch{" ++ [a] ++ "}"
      VInteger a -> show a
      VWord a -> "/" ++ a
      VKeyword a -> ":" ++ a
      VBool False -> "f"
      VBool True -> "t"
      VQuot _ -> "[QUOT]"
      VClosure vals _ -> "[closure over " ++ (intercalate " " $ map prettyprint vals) ++ "]"
      VCons h t -> prettylist [] h t
      VTable t -> "T{" ++ (intercalate " " $ map pptable $ M.toList t) ++ "}"

    where pptable (k,v) = (prettyprint k) ++ " " ++ (prettyprint v)

doprettyprint = popstack >>= (liftIO . putStrLn . prettyprint)

checkedString = do
  v <- popstack
  case v of
    VString s -> return s
    _ -> chiError "Expecting String"

checkedChar = do
  v <- popstack
  case v of
    VChar s -> return s
    _ -> chiError "Expecting Character"

writestr = checkedString >>= (liftIO . putStr)

toString = do
  v <- popstack
  pushstack $ VString $ case v of
                          VString a -> a
                          a -> prettyprint a

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
    _ -> chiError $ "Expecting list, got " ++ (prettyprint v)

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

tableKeys = do
  t <- checkedTable
  pushstack $ foldr VCons (VBool False) $ M.keys t

tableValues = do
  t <- checkedTable
  pushstack $ foldr VCons (VBool False) $ M.elems t

tableUnion = do
  t1 <- checkedTable
  t2 <- checkedTable
  pushstack $ VTable $ M.union t1 t2



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

          , ("<<",     inserttable)
          , (">>",     gettable)
          , ("keys",   tableKeys)
          , ("values", tableValues)
          , ("union",  tableUnion)

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
