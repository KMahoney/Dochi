module Dochi.Core ( prettyprint
                  , checkedString
                  , checkedChar
                  , checkedInteger
                  , checkedCons
                  , checkedTable
                  , coreState
                  ) where

import Dochi.IMC
import Dochi.Interpreter

import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad.State
import Random



-- strings


prettyprint :: Value -> String
prettyprint v = 
    case v of
      VString a -> "\"" ++ a ++ "\""
      VChar a -> "Ch{" ++ [a] ++ "}"
      VInteger a -> show a
      VWord a -> ":" ++ a
      VKeyword a -> ":" ++ a
      VBool False -> "f"
      VBool True -> "t"
      VQuot _ -> "[QUOT]"
      VClosure vals _ -> "[closure over " ++ (intercalate " " $ map prettyprint vals) ++ "]"
      VCons h t -> prettylist [] h t
      VTable t -> "T{" ++ (intercalate " " $ map prettytable $ M.toList t) ++ "}"

    where prettytable (k,v) = (prettyprint k) ++ " " ++ (prettyprint v)
          prettylist li h (VCons h2 t2) = prettylist (h:li) h2 t2
          prettylist li h (VBool False) = "L{" ++ (intercalate " " $ map prettyprint $ reverse $ h:li) ++ "}"
          prettylist li h t = "C{" ++ (intercalate " " $ map prettyprint $ reverse $ t:h:li) ++ "}"

doprettyprint = popstack >>= (liftIO . putStrLn . prettyprint)

checkedString :: Chi String
checkedString = do
  v <- popstack
  case v of
    VString s -> return s
    _ -> chiError "Expecting String"

checkedChar :: Chi Char
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


-- numbers

checkedInteger :: Chi Integer
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

checkedCons :: Chi (Value, Value)
checkedCons = do
  v <- popstack
  case v of
    VCons h t -> return (h, t)
    _ -> chiError $ "Expecting list, got " ++ (prettyprint v)

listCons = do
  h <- popstack
  t <- popstack
  pushstack $ VCons h t

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


-- tables

checkedTable :: Chi (M.Map Value Value)
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


tablelib = M.fromList
           [ ("<<",     inserttable)
           , (">>",     gettable)
           , ("keys",   tableKeys)
           , ("values", tableValues)
           , ("union",  tableUnion)
           ]

listlib = M.fromList
          [ (";",      listCons)
          , ("head",   listHead)
          , ("tail",   listTail)
          , ("length", listLength)
          , ("nth",    listNth)
          ]

dochilib = M.fromList []

corelib = M.fromList
          [ ("pp", doprettyprint)
          , ("write", writestr)
          , ("->string", toString)
          , ("clear", clearstack)

          , ("if", ifstmt)

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


-- |Inject core libraries into the interpreter state

coreState :: ChiState -> ChiState
coreState =   injectLib "core" corelib
            . injectLib "dochi" dochilib
            . injectLib "table" tablelib
            . injectLib "list" listlib
