module IC where

import Parse (AST(..))
import Data.List (elemIndex, (\\))
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error

-- To manipulate Haskell data structures with the interpreter, replace
-- ForeignType with a type of your choosing. There is probably a better,
-- more general way to do this e.g. with existential quantification, but I
-- don't understand the type system well enough.

-- A parametrised Value type would work, but the parameter cascades down
-- through IC etc. making it a lot of work.

type ForeignType = ()

data Value = VWord String
           | VKeyword String
           | VInteger Integer
           | VString String
           | VQuot [IC]
           | VClosure [Value] [IC]
           | VTrue
           | VNil
           | VCons Value Value
           | VTable (M.Map Value Value)
           | ForeignValue ForeignType
             deriving (Show, Eq, Ord)

-- Intermediate Code

-- Captured values are pushed to a separate stack with VarPush
-- and referenced with VarIndex. At the end of the scope they are
-- pushed off the stack with EndScope. The rest are fairly
-- self-explanatory.

data IC = CallWord String
        | FnCall
        | PushValue Value
        | PopValue
        | VarPush String
        | EndScope Integer
        | VarIndex Integer
        | MakeClosure [Int] [IC]
          deriving (Show, Eq, Ord)


type VarState = [String]


-- uses a Writer monad to output IC

type Compiler a = WriterT [IC] (StateT VarState (Either String)) a


freevars :: [AST] -> S.Set String
freevars [] = S.empty
freevars (h:t) =
    case h of
      Word s      -> S.insert s (freevars t)
      CodeBlock a -> S.union (freevars a) (freevars t)
      CallBlock a -> S.union (freevars [a]) (freevars t)
      Capture a   -> (freevars t) S.\\ (S.fromList a)
      LList a     -> S.union (freevars a) (freevars t)
      LTable a    -> S.union (freevars a) (freevars t)
      _           -> freevars t

compileClosure :: [AST] -> Compiler ()
compileClosure ast =
    do st <- get
       if null (captured st)
         then case compileScoped [] ast of
                Left err -> throwError err
                Right quot -> tell [PushValue $ VQuot quot]
         else case compileScoped (captured st) ast of
                Left err -> throwError err
                Right quot -> tell [MakeClosure (indexes st) quot]

    where captured st = S.toList $ S.intersection (freevars ast) (S.fromList st)
          indexes st = reverse $ catMaybes $ map (flip elemIndex st) (captured st)


-- Literal list and table constructs

literalValue :: AST -> Compiler Value
literalValue v =
    case v of
      Word "nil"      -> return VNil
      Word name       -> return $ VWord name
      LInteger value  -> return $ VInteger value
      LString value   -> return $ VString value
      LKeyword value  -> return $ VKeyword value
      LList value     -> literalList value
      LTable value    -> literalTable value

      CodeBlock ast   -> do vs <- get
                            case compile vs ast of
                              Left err -> throwError err
                              Right quot -> return $ VQuot quot

      -- errors
      Capture ids     -> throwError "Capture in literal list"
      CallBlock value -> throwError "@ Call in literal list"


literalList :: [AST] -> Compiler Value
literalList ast = mapM literalValue ast >>= (return . foldr VCons VNil)

literalTable :: [AST] -> Compiler Value
literalTable ast = do t <- mapM literalValue ast
                      m <- makeMap t
                      return $ VTable m

    where makeMap :: [Value] -> Compiler (M.Map Value Value)
          makeMap [] = return M.empty
          makeMap (_:[]) = throwError "Odd number of values for literal table"
          makeMap (k:v:tail) = makeMap tail >>= (return . M.insert k v)



compileAST :: AST -> Compiler ()
compileAST ast =

    case ast of
      Word name       -> callword name
      LInteger value  -> tell [PushValue $ VInteger value]
      LString value   -> tell [PushValue $ VString value]
      LKeyword value  -> tell [PushValue $ VKeyword value]
      Capture ids     -> do tell $ map VarPush (reverse ids)
                            modify $ \st -> ids ++ st
      CodeBlock ast   -> compileClosure ast
      CallBlock value -> do compileAST value
                            tell [FnCall]
      LList value     -> do l <- literalList value
                            tell [PushValue l]
      LTable value    -> do t <- literalTable value
                            tell [PushValue t]

    where 
          callword "drop" = tell [PopValue]
          callword "nil"  = tell [PushValue VNil]
          callword "call" = tell [FnCall]
          callword "true" = tell [PushValue VTrue]
          callword "false" = tell [PushValue VNil]
          callword name = do st <- get
                             case (elemIndex name st) of
                               Just i -> tell [VarIndex (toInteger i)]
                               Nothing -> tell [CallWord name]





runCompiler st action = evalStateT (execWriterT $ action) st

compile :: VarState -> [AST] -> Either String [IC]
compile st ast = runCompiler st $ mapM_ compileAST ast


-- Compile the AST and pop the captured values from the value stack.

compileScoped :: VarState -> [AST] -> Either String [IC]
compileScoped st ast = runCompiler st $ do mapM_ compileAST ast
                                           st <- get
                                           let c = length st
                                           when (c > 0) $ tell [EndScope $ toInteger c]
