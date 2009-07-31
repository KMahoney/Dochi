module Dochi.Compile (envCompile) where

import Data.List (elemIndex, (\\))
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error

import Dochi.IR
import Dochi.Parse (AST(..))

data CompileState = CompileState { varList :: [String]
                                 , envList :: [ (String, [String]) ]
                                 , useList :: [String]
                                 }

-- uses a Writer monad to output IR

type Compiler a = WriterT [IR] (StateT CompileState (Either String)) a


newCompileState = CompileState [] [] []

freevars :: [AST] -> S.Set String
freevars [] = S.empty
freevars (h:t) =
    case h of
      Word s      -> S.insert s (freevars t)
      CodeBlock a -> S.union (freevars a) (freevars t)
      BuildList a -> S.union (freevars a) (freevars t)
      CallBlock a -> S.union (freevars [a]) (freevars t)
      Capture a   -> (freevars t) S.\\ (S.fromList a)
      _           -> freevars t

compileClosure :: [AST] -> Compiler ()
compileClosure ast =
    do CompileState vars e u <- get
       if null (captured vars)
         then case compileScoped (CompileState [] e u) ast of
                Left err -> throwError err
                Right quot -> tell [PushValue $ VQuot quot]
         else case compileScoped (CompileState (captured vars) e u) ast of
                Left err -> throwError err
                Right quot -> tell [MakeClosure (indexes vars) quot]

    where captured st = S.toList $ S.intersection (freevars ast) (S.fromList st)
          indexes st = reverse $ catMaybes $ map (flip elemIndex st) (captured st)


-- Literal list and table constructs

literalValue :: AST -> Compiler Value
literalValue v =
    case v of
      Word "f"        -> return $ VBool False
      Word "t"        -> return $ VBool True

      LInteger value  -> return $ VInteger value
      LString value   -> return $ VString value
      LChar value     -> return $ VChar value
      LKeyword value  -> return $ VKeyword value
      LList value     -> literalList value
      LCons value     -> literalCons value
      LTable value    -> literalTable value

      CodeBlock ast   -> do st <- get
                            case compileScoped st ast of
                              Left err -> throwError err
                              Right quot -> return $ VQuot quot

      -- errors
      Word _          -> throwError "Word not literal"
      ModWord _ _     -> throwError "Word not literal"
      BuildList _     -> throwError "List not literal"
      Capture ids     -> throwError "Capture in literal list"
      CallBlock value -> throwError "@ Call in literal list"


literalList :: [AST] -> Compiler Value
literalList ast = mapM literalValue ast >>= (return . foldr VCons (VBool False))

literalCons :: [AST] -> Compiler Value
literalCons ast = if length ast < 2
                    then throwError "Literal cons must contain at least 2 values"
                    else mapM literalValue ast >>= (return . foldr1 VCons)

literalTable :: [AST] -> Compiler Value
literalTable ast = do t <- mapM literalValue ast
                      m <- makeMap t
                      return $ VTable m

    where makeMap :: [Value] -> Compiler (M.Map Value Value)
          makeMap [] = return M.empty
          makeMap (_:[]) = throwError "Odd number of values for literal table"
          makeMap (k:v:tail) = makeMap tail >>= (return . M.insert k v)



buildList :: [AST] -> Compiler ()
buildList ast = tell [PushValue $ VBool False] >> mapM_ f (reverse ast)
    where f ast = do compileAST ast
                     tell [CallWord "list" ";"]

callword :: String -> Compiler ()

callword "drop" = tell [PopValue]
callword "f"    = tell [PushValue $ VBool False]
callword "t"    = tell [PushValue $ VBool True]
callword "call" = tell [FnCall]

callword name   = do vars <- gets varList
                     case (elemIndex name vars) of
                       Just i -> tell [VarIndex (toInteger i)]
                       Nothing -> findModule name


findModule :: String -> Compiler ()
findModule name = do CompileState _ e u <- get
                     case f (u ++ defaultModules) e of
                       [m] -> tell [CallWord m name]
                       []  -> throwError $ "Unknown word " ++ name
                       l   -> throwError $ "Conflict: word " ++ name ++ " in " ++ (show l)
    where f u = map fst . filter (elem name . snd) . filter (flip elem u . fst)
          defaultModules = ["core"]


compileAST :: AST -> Compiler ()
compileAST ast =

    case ast of
      Word name       -> callword name
      ModWord m name  -> tell [CallWord m name]
      LInteger value  -> tell [PushValue $ VInteger value]
      LString value   -> tell [PushValue $ VString value]
      LChar value     -> tell [PushValue $ VChar value]
      LKeyword value  -> tell [PushValue $ VKeyword value]
      Capture ids     -> do tell $ map VarPush (reverse ids)
                            modify $ \(CompileState vars e u) -> CompileState (ids ++ vars) e u
      CodeBlock ast   -> compileClosure ast
      CallBlock value -> do compileAST value
                            tell [FnCall]

      BuildList ast   -> buildList ast

      LList value     -> do l <- literalList value
                            tell [PushValue l]
      LCons value     -> do l <- literalCons value
                            tell [PushValue l]
      LTable value    -> do t <- literalTable value
                            tell [PushValue t]




runCompiler st action = evalStateT (execWriterT $ action) st

compile :: CompileState -> [AST] -> Either String [IR]
compile st ast = runCompiler st $ mapM_ compileAST ast


-- Compile the AST and pop the captured values from the value stack.

compileScoped :: CompileState -> [AST] -> Either String [IR]
compileScoped st ast = runCompiler st $ do mapM_ compileAST ast
                                           st' <- gets varList
                                           let c = length st'
                                           when (c > 0) $ tell [EndScope $ toInteger c]



-- |Compile with provided environment

envCompile :: [(String, [String])] -- ^ environment
           -> [String]             -- ^ list of useable modules
           -> [AST]                -- ^ Syntax tree to compile
           -> Either String [IR]   -- ^ Returns intermediate code

envCompile e u = compileScoped (CompileState [] e u)
