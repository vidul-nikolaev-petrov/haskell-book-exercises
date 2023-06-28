module Main where

lefts' :: [Either a b] -> [a]
lefts' = foldr left []
  where
    left (Left x) b = x : b
    left _ b = b

rights' :: [Either a b] -> [b]
rights' = foldr right []
  where
    right (Right x) b = x : b
    right _ b = b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

main :: IO ()
main = do
  print $ lefts' [Right "succ_1", Left "err_1", Left "err_2"]
  print $ rights' [Left "err_1", Right "succ_1", Right "succ_2"]
  print $ partitionEithers' [Right "succ_1", Left "err_1", Right "succ_2", Left "err_2"]
  print $ eitherMaybe' (+ 1) (Left 1)
  print $ eitherMaybe' (+ 1) (Right 1)
  print $ either' (1 -) (+ 1) (Left 1)
  print $ either' (1 -) (+ 1) (Right 1)
  print $ eitherMaybe'' (+ 1) (Right 1)