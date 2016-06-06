{-# OPTIONS_GHC -Wall #-}

--------- Implementation of creating an AVL tree from a list -------
-- NOTE: Refer http://www.geeksforgeeks.org/avl-tree-set-1-insertion/ for theory
-- behind AVL tree insertion and for understading the terms and conventions
-- used in this implementation

data Tree a = 
   Leaf 
   | Node { h::Integer, l::(Tree a), m::a, r::(Tree a) }
   deriving (Show, Eq)

data ImbalanceType =
   None
   | LeftLeft
   | LeftRight
   | RightLeft
   | RightRight
   deriving (Show, Eq)

-- | @foldTree xs@ performs right folding on @xs@ to create a balanced AVL tree
foldTree :: (Ord a) => [a] -> Tree a
foldTree xs = foldr (\x t -> insert x t) Leaf xs

-- | @insert x t@ inserts element @x@ in @t@ and re-balances if necessary
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node 1 Leaf x Leaf
insert x t
   | x < (m t)  = balance x (
                  Node 
                  (max (h t) (h (insert x (l t)) + 1)) 
                  (insert x (l t)) 
                  (m t) 
                  (r t)
                  )
   | x > (m t)  = balance x (
                  Node 
                  (max (h t) (h (insert x (r t)) + 1)) 
                  (l t)
                  (m t)
                  (insert x (r t))
                  )
   | otherwise  = t

-- | @balance x t@ balances the tree @t@ just after inserting @x@
--   This is not a generic balance function. Should be only called from within
--   insert 
balance :: (Ord a) => a -> Tree a -> Tree a
balance _ Leaf = Leaf
balance x t@(Node h_ a b c)
   | imbalance x t == LeftLeft   = rotateR t
   | imbalance x t == LeftRight  = rotateR (Node h_ (rotateL a) b c )
   | imbalance x t == RightRight = rotateL t
   | imbalance x t == RightLeft  = rotateL (Node h_ a b (rotateR c) )
   | otherwise                   = t -- No imbalance

-- | @imbalance x t@ finds out the type of imbalance created just after
--   inserting @x@ in @t@
imbalance :: (Ord a) => a -> Tree a -> ImbalanceType
imbalance x t = if (abs (balanceFactor t) <= 1) then None else y
   where y = if (balanceFactor t) > 0 
             then ( if x < m (l t) then LeftLeft else LeftRight )
             else ( if x > m (r t) then RightRight else RightLeft )

-- | balanceFactor t@ calculates a numerical value of balance at root @t@
--   Negative value represents that the tree is heavier on the right
--   Positive value represents that the tree is heavier on the left
--   Zero represents a balanced state
balanceFactor :: Tree a -> Integer
balanceFactor (Node _ Leaf _ Leaf)   = 0
balanceFactor (Node _ Leaf _ r_)     = -(h r_)
balanceFactor (Node _ l_ _ Leaf)     = h l_
balanceFactor t                      = h (l t) - h (r t)

-- | rotateR t@ performs a right-rotation of subtree rooted at @t@
--   See note at beginning of file
rotateR :: Tree a -> Tree a
rotateR t = 
   let x = l (l t)
       y = l t
       t3 = r y
       z = t
       t4 = r z
       h'' = max (h' t3) (h' t4) 
   in Node (h'' + 2) (x) (m y) (Node (h'' + 1) t3 (m z) t4)

-- | rotateL t@ performs a left-rotation of subtree rooted at @t@
--   See note at beginning of file
rotateL :: Tree a -> Tree a
rotateL t =
   let x = r (r t)
       y = r z
       z = t
       t1 = l z
       t2 = l y
       h'' = max (h' t1) (h' t2) 
   in Node (h'' + 2) (Node (h'' + 1) t1 (m z) t2) (m y) (x)

-- | @h'@ is a utility function to give height of any node
h' :: Tree a -> Integer
h' Leaf = 0
h' t = h t

-- | @str t@ gives out an easy to visualize representation of a tree
--   Useful for debugging
str :: (Show a) => Tree a -> String
str Leaf = "x"
str (Node _ ll mm rr) = show mm ++ "(" ++ str ll ++ "," ++ str rr ++ ")"

