```haskell
data BinaryTree a = 
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)    
```

Given the definition of `BinaryTree` we have provided, write a catamorphism for the binary trees.

```haskell
-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
```

**Answer**
```haskell
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f (foldTree f acc left) right)
```
