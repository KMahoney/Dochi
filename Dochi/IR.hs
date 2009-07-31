module Dochi.IR where

import qualified Data.Map as M
import Data.Dynamic


-- Quick hack
instance Eq Dynamic where _ == _ = False
instance Ord Dynamic where _ < _ = False

-- |Intermediate Representation
-- 
-- Captured values are pushed to a separate stack with VarPush
-- and referenced with VarIndex. At the end of the scope they are
-- pushed off the stack with EndScope. The rest are fairly
-- self-explanatory.

data Value = VWord String
           | VKeyword String
           | VInteger Integer
           | VString String
           | VChar Char
           | VQuot [IR]
           | VClosure [Value] [IR]
           | VBool Bool
           | VCons Value Value
           | VTable (M.Map Value Value)
           | VDyn Dynamic
             deriving (Show, Eq, Ord)

data IR = CallWord String String
        | FnCall
        | PushValue Value
        | PopValue
        | VarPush String
        | EndScope Integer
        | VarIndex Integer
        | MakeClosure [Int] [IR]
          deriving (Show, Eq, Ord)
