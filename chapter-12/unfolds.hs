module Main where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = result (f x) : myUnfoldr f (result' (f x))
  where
    result (Just (a, b)) = a
    result' (Just (a, b)) = b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

main :: IO ()
main = do
  print $ take 10 $ myIterate (+ 1) 0
  print $ take 10 $ myUnfoldr (\b -> Just (b, b + 1)) 0
  print $ take 10 $ betterIterate (+ 1) 0