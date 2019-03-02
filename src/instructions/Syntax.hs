{-#LANGUAGE GADTs, StandaloneDeriving #-}

module Syntax where

import ISMonad

type Program = [Statement] 

data Statement = Assign Exp Exp
               | Return Exp
               deriving (Show)

data Exp = I Int
         | V Ident
         | IBinary Op Exp Exp
         deriving (Show)

data Op = Add | Minus | Multiply | Divide
             deriving (Show)

type ASMProgram = [ASMInstr]

data ASMInstr = Move (ASMOperand Dest) (ASMOperand Source)
              | Ret
              deriving (Show, Eq)

data Source = Source deriving (Show, Eq)
data Dest = Dest deriving (Show, Eq)

data ASMOperand a where
  BinComp :: Op -> ASMOperand Source -> ASMOperand Source -> ASMOperand Source
  ICons   :: Int -> ASMOperand Source
  Reg     :: Ident -> ASMOperand Dest
  Temp    :: Ident -> ASMOperand Source

deriving instance Show a => Show (ASMOperand a)
deriving instance Eq a => Eq (ASMOperand a) 