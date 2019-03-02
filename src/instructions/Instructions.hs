module Instructions where 

import Syntax 
import ISMonad 

cogenProgram :: Program -> ASMProgram -> ASMProgram
cogenProgram [] acc = acc
cogenProgram prog acc = foldl (\a s -> a ++ (cogenStm s)) [] prog

-- | MAXIMAL MUNCH, passing down return register 
cogenStm :: Statement -> ASMProgram
cogenStm (Assign (V x) e) = cogen x e
cogenStm (Return e)   = (cogen "v0" e) ++ [Ret]

cogen :: Ident -> Exp -> ASMProgram
cogen r (I x) = [Move (Reg r) (ICons x)]
cogen r (V x) = [Move (Reg r) (Temp x)]
cogen r (IBinary op e1 e2) = let t1 = r ++ "'"
                                 t2 = r ++ "+"
                             in
                               (cogen t1 e1) ++ (cogen t2 e2) ++ [Move (Reg r) (BinComp op (Temp t1) (Temp t2))]

cogenProgram' :: Program -> ASMProgram
cogenProgram' p = runIS . (cogenProgramUp p) >>= ssa

-- | Transform instructions into SSA form

ssa :: ASMProgram -> IS ASMProgram
ssa [] = return []
ssa ((Move (Reg r) e):xs) = do (y:ys) <- propOperand r xs 
                               ys'    <- ssa ys
                               return ([(Move r e), y] ++ ys)
ssa (x:xs) = (ssa xs) >>= (\p -> (x:p))

propOperand :: Ident -> ASMProgram -> IS ASMProgram
propOperand v ((Move (Reg r) e):xs)
    | v == r = do x'  <- freshRegister
                  xs' <- propOperand (Reg x') (substProgram v x' xs)
                  return (x:xs)
    | otherwise = (Move r (substOperand e v)) : propOperand v xs

substProgram :: Ident -> Ident -> ASMProgram -> ASMProgram
substProgram match sub ((Move (Reg r) e):xs)
    | match == r = (Move (Reg r) (substOperand match sub e)):xs
    | otherwise  = [Move (Reg r) (substOperand match sub e)] ++ substProgram match sub xs
substProgram _ _ xs = xs

substOperand :: ASMOperand Source -> Ident -> ASMOperand Source
substOperand (Temp x) v sub
    | x == v = sub
    | otherwise = (Temp x)
substOperand (BinComp op l r) v sub = BinComp op (substOperand l v sub) (substOperand r v sub)
substOperand s _ _ = s

-- | Generating instructions

cogenProgramUp :: Program -> IS ASMProgram
cogenProgramUp [] = return []
cogenProgramUp (x:xs) = do p  <- cogenStmUp x
                           ps <- cogenProgramUp xs
                           return (p ++ ps)
                               

cogenStmUp :: Statement -> IS ASMProgram
cogenStmUp (Assign (V x) e) = do (v,p) <- cogenUp e
                                 return (p ++ [Move (Reg x) v])
cogenStmUp (Return e) = do (v,p) <- cogenUp e
                           return (p ++ [Move (Reg "v0") v, Ret])

cogenUp :: Exp -> IS (ASMOperand Source, ASMProgram)
cogenUp (I x) = return (ICons x, [])
cogenUp (V x) = return (Temp x, [])
cogenUp (IBinary op e1 e2) = do v <- freshRegister
                                (v1,p1) <- cogenUp e1
                                (v2,p2) <- cogenUp e2
                                return (Temp v, p1 ++ p2 ++ [Move (Reg v) (BinComp op v1 v2)])

showASM :: ASMProgram -> String
showASM xs = foldr (\s a -> a ++ (showASMInstr s)) "r:" xs

showASMInstr (Move e1 e2) = (showASMOp e1) ++ "<-" ++ (showASMOp e2) ++ "\n"
showASMInstr (Ret) = "Ret"

showASMOp :: ASMOperand a -> String
showASMOp (BinComp op e1 e2) = (showASMOp e1) ++ (show op) ++ (showASMOp e2)
showASMOp (ICons v) = show v
showASMOp (Reg x) = x
showASMOp (Temp x) = x