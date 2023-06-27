module Main where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

main :: IO ()
main = do
  print $ natToInteger Zero
  print $ natToInteger (Succ Zero)
  print $ natToInteger (Succ (Succ Zero))