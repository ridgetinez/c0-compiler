module Properties where

import Syntax
import Test.QuickCheck

-- | Test coverage:
-- |   + Unit tests on expressions -> ASM
-- |   + Property tests on length of the generated ASM program

-- | Compute the number of ASM instructions generated via naive maximal munch
numASMInstructions :: Program -> Int
numASMInstructions [] = 0
numASMInstructions ((Return e):stmts) = 2 + numASMInstructions stmts
numASMInstructions ((Assign dest val):stmts) = numASMExpInstructions val + numASMInstructions stmts

numASMExpInstructions :: Exp -> Int
numASMExpInstructions (I x) = 1
numASMExpInstructions (V x) = 1
numASMExpInstructions (IBinary _ e1 e2) = 1 + (numASMExpInstructions e1)
                                            + (numASMExpInstructions e2)