module Compile where

import Data.List (elemIndex, (\\))
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error

import IMC
import Parse (AST(..))

data CompileState = CompileState { varState :: [String]
                                 , mLookup :: M.Map String String
                                 }

-- uses a Writer monad to output IC

type Compiler a = WriterT [IC] (StateT CompileState (Either String)) a


newCompileState = CompileState [] M.empty

freevars :: [AST] -> S.Set String
freevars [] = S.empty
freevars (h:t) =
    case h of
      Word s      -> S.insert s (freevars t)
      CodeBlock a -> S.union (freevars a) (freevars t)
      CallBlock a -> S.union (freevars [a]) (freevars t)
      Capture a   -> (freevars t) S.\\ (S.fromList a)
      _           -> freevars t

compileClosure :: [AST] -> Compiler ()
compileClosure ast =
    do CompileState st e <- get
       if null (captured st)
         then case compileScoped (CompileState [] e) ast of
                Left err -> throwError err
                Right quot -> tell [PushValue $ VQuot quot]
         else case compileScoped (CompileState (captured st) e) ast of
                Left err -> throwError err
                Right quot -> tell [MakeClosure (indexes st) quot]

    where captured st = S.toList $ S.intersection (freevars ast) (S.fromList st)
          indexes st = reverse $ catMaybes $ map (flip elemIndex st) (captured st)


-- Literal list and table constructs

literalValue :: AST -> Compiler Value
literalValue v =
    case v of
      Word "f"        -> return $ VBool False
      Word "t"        -> return $ VBool True
      Word name       -> return $ VWord name
      LInteger value  -> return $ VInteger value
      LString value   -> return $ VString value
      LKeyword value  -> return $ VKeyword value
      LList value     -> literalList value
      LTable value    -> literalTable value

      CodeBlock ast   -> do st <- get
                            case compile st ast of
                              Left err -> throwError err
                              Right quot -> return $ VQuot quot

      -- errors
      Capture ids     -> throwError "Capture in literal list"
      CallBlock value -> throwError "@ Call in literal list"


literalList :: [AST] -> Compiler Value
literalList ast = mapM literalValue ast >>= (return . foldr VCons (VBool False))

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
                            modify $ \(CompileState st e) -> (CompileState (ids ++ st) e)
      CodeBlock ast   -> compileClosure ast
      CallBlock value -> do compileAST value
                            tell [FnCall]
      LList value     -> do l <- literalList value
                            tell [PushValue l]
      LTable value    -> do t <- literalTable value
                            tell [PushValue t]

    where 
          callword "drop" = tell [PopValue]
          callword "f"    = tell [PushValue $ VBool False]
          callword "t"    = tell [PushValue $ VBool True]
          callword "call" = tell [FnCall]
          callword name = do CompileState st m <- get
                             case (elemIndex name st) of
                               Just i -> tell [VarIndex (toInteger i)]
                               Nothing -> case M.lookup name m of
                                            Just m' -> tell [CallWord m' name]
                                            Nothing -> throwError $ "Unknown word " ++ name


runCompiler st action = evalStateT (execWriterT $ action) st

compile :: CompileState -> [AST] -> Either String [IC]
compile st ast = runCompiler st $ mapM_ compileAST ast


-- Compile the AST and pop the captured values from the value stack.

compileScoped :: CompileState -> [AST] -> Either String [IC]
compileScoped st ast = runCompiler st $ do mapM_ compileAST ast
                                           st' <- gets varState
                                           let c = length st'
                                           when (c > 0) $ tell [EndScope $ toInteger c]

envCompile e = compileScoped (CompileState [] e)
