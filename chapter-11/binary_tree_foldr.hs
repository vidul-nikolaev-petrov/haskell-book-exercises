data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

t5 :: BinaryTree Integer
t5 = Node (Node (Node Leaf 1 Leaf) 3 Leaf) 5 (Node (Node Leaf 6 Leaf) 7 (Node Leaf 9 Leaf))

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f (foldTree f acc left) right)

main :: IO ()
main = do
  print $ foldTree (+) 0 t5