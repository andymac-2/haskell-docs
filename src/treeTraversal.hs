module TreeTraversal where

-- Trees are data structures, which are often used in computer science.
-- Read more about them here: https://en.wikipedia.org/wiki/Tree_(data_structure)

-- In Haskell this can neatly be represented by a recursive algebraic data type.
-- One common function used on trees is traversal, which visits all parts of the tree.
-- It does so, most commonly, in one of three ways; pre-order, in-order, or post-order.
-- Read more about it here: https://en.wikipedia.org/wiki/Tree_traversal

-- There could be several exercises around this subject. Here are some suggestions:

-- 1. Represent a Tree as a recursive algebraic data type.
--    Each node or leaf should contain a single value. (Fx a character or a number).

-- 2. Create a tree of your choosing using your algebraic data type.

-- 3. Implement one version of tree traversal. (pre, in, post).
--    It should collect the visited values in a list, which it will return.

-- 4. Write a simple test function, which validates the correctness of your traversal implementation.
--    It does not need to be generic. Just check the returned values of a known tree.

-- 5. Represent the different traversal orders as an algebraic data type and make
--    a function with the following type signature: traverseTree :: TraverseOrder -> Tree a -> [a]

-- 6. Implement a mapping function for your tree with the following signature: mapTree :: (a -> b) -> Tree a -> Tree b


-- 1.
data Tree a =
    Nil
  | Leaf a
  | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

-- 2. to 5.
data TraverseOrder =
    PreOrder
  | InOrder
  | PostOrder
  deriving (Eq, Show)

traverseTree :: TraverseOrder -> Tree a -> [a]
traverseTree _ Nil = []
traverseTree _ (Leaf x) = [x]
traverseTree order (Node x left right) =
  case order of
    PreOrder -> [x] ++ traverseTree order left ++ traverseTree order right
    InOrder -> traverseTree order left ++ [x] ++ traverseTree order right
    PostOrder -> traverseTree order left ++ traverseTree order right ++ [x]


exampleTree :: Tree Char
exampleTree = (Node 'F'
                (Node 'B'
                  (Leaf 'A')
                  (Node 'D'
                    (Leaf 'C')
                    (Leaf 'E')
                  )
                )
                (Node 'G'
                  Nil
                  (Node 'I'
                    (Leaf 'H')
                    Nil
                  )
                )
              )

traversalOrdersAreCorrectOnExampleTree =
  preOrderIsCorrect && inOrderIsCorrect && postOrderIsCorrect
  where
    preOrderIsCorrect =
      traverseTree PreOrder exampleTree  == ['F', 'B', 'A', 'D', 'C', 'E', 'G', 'I', 'H']

    inOrderIsCorrect =
      traverseTree InOrder exampleTree   == ['A'..'I']

    postOrderIsCorrect =
      traverseTree PostOrder exampleTree == ['A', 'C', 'E', 'D', 'B', 'H', 'I', 'G', 'F']


-- 6.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil = Nil
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node a left right) = Node (f a) (mapTree f left) (mapTree f right)
