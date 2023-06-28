module Main where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x | x < 0 = Nothing
integerToNat x = Just $ toNat x
  where
    toNat x
      | x == 0 = Zero
      | otherwise = Succ (toNat (x - 1))

main :: IO ()
main = do
  print $ natToInteger Zero
  print $ natToInteger (Succ Zero)
  print $ natToInteger (Succ (Succ Zero))
  print $ integerToNat (-1)
  print $ integerToNat 0
  print $ integerToNat 8