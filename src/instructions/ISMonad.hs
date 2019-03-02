module ISMonad where

import Control.Applicative
import Control.Monad

type Ident = String

{-
  IS Monad - Instruction Selection monad used to thread a list of available identifiers
             through computations. Currently used to retrieve fresh variable names.
-}

newtype IS a = IS ([Ident] -> ([Ident], a))

instance Monad IS where
  return x = IS $ \xs -> (xs, x)
  (IS g) >>= f = IS $ \xs -> case g xs of (xs', a) -> case f a of IS g' -> g' xs' 

instance Applicative IS where
  pure = return
  (<*>) = ap

instance Functor IS where
  fmap g (IS f) = IS $ \xs -> let rest = f xs in (fst rest, g $ snd rest)

runIS :: IS a -> a
runIS (IS f) = snd $ f freshNames

freshNames :: [Ident]
freshNames = map pure ['a'..'z'] ++ map ((++) "a" . show) [1..]

freshRegister :: IS Ident
freshRegister = IS $ \(x:xs) -> (xs, x)