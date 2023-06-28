module Main where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Just (l, b, r) -> Node (unfold f l) b (unfold f r)
    Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (f n) 0
  where
    f :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
    f n i
      | n > i = Just (i + 1, i, i + 1)
      | otherwise = Nothing

treeBuild' :: Integer -> BinaryTree Integer
treeBuild' n = unfold f 0
  where
    f i
      | i < n = Just (i + 1, i, i + 1)
      | otherwise = Nothing

main :: IO ()
main = do
  print $ treeBuild 3
  print $ treeBuild' 3