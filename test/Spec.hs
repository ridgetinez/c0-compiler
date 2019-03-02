module Main where

import Syntax
import ISMonad
import Test.Hspec
import Data.List
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Retrieving fresh variables" $ do
        it "Calling freshRegister 3 times retrieves registers: 'a', 'b', 'c'" $ do
            (concatFreshVariables 3) `shouldBe` "c b a"
        it "All finite runs of freshRegister give back unique register names" $ 
            property $ \n -> let regs = runIS $ freshVariables (n :: Int) in regs == nub regs
    {-
    describe "Building instructions (passing registers up)" $ do
        it "z = (x+3) * (y-5), return z" $ do
            shouldBe (cogenProgram' [Assign (V z) (IBinary Multiply (IBinary Add (V x) (I 3)) (IBinary Minus (V y) (I 5))),
                           Return (V z)])
                     ([Move (Reg "a") (BinComp Plus (Temp "x") (ICons 3))])
    -}


concatFreshVariables :: Int -> String
concatFreshVariables n = runIS $ do
    xs <- freshVariables n
    return $ intercalate " " xs

freshVariables :: Int -> IS [Ident]
freshVariables n = f $ map (const freshRegister) [1..n] 

f :: Monad m => [m a] -> m [a]
f [] = return []
f (x:xs) = do ms <- f xs
              x' <- x
              return (x':ms)