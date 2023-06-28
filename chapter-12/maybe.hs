module Main where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs
  | any (== Nothing) xs = Nothing
  | otherwise = Just $ catMaybes xs

main :: IO ()
main = do
  print $ isJust Nothing
  print $ isJust (Just 1)
  print $ isNothing Nothing
  print $ isNothing (Just 1)
  print $ mayybee 0 (+ 1) Nothing
  print $ mayybee 0 (+ 1) (Just 1)
  print $ fromMaybe 0 Nothing
  print $ fromMaybe 0 (Just 1)
  print $ listToMaybe ([] :: [Int])
  print $ listToMaybe [1, 2, 3]
  print $ maybeToList (Nothing :: Maybe Int)
  print $ maybeToList (Just 1)
  print $ catMaybes [Just 1, Nothing, Just 2]
  print $ catMaybes . take 3 $ repeat (Nothing :: Maybe Int)
  print $ flipMaybe [Just 1, Nothing, Just 3]
  print $ flipMaybe [Just 1, Just 2, Just 3]