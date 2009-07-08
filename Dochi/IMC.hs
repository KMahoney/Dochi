module IMC where

import qualified Data.Map as M

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

data IC = CallWord String String
        | FnCall
        | PushValue Value
        | PopValue
        | VarPush String
        | EndScope Integer
        | VarIndex Integer
        | MakeClosure [Int] [IC]
          deriving (Show, Eq, Ord)
