{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

-- | @glCons e gl@ Added emploee @e@ to the guestlist @gl@
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (empFun e + f)

-- | Monoid instance of GuestList
instance Monoid GuestList where
   mempty = GL [] 0
   mappend (GL as f1) (GL bs f2) = (GL (as++bs) (f1 + f2))

-- | @moreFun@ Compares which of the two guestlist is more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ f1) b@(GL _ f2)
   | f1 > f2   = a
   | otherwise = b

-- | @foldTree f g k t@ folds a tree @t@ by applying function @f@ on the leaf nodes,
--   function @g@ on the non-leaf nodes and starting with constant @k@
foldTree :: (b -> Tree a -> b) -> ([b] -> Tree a -> b) -> b ->  Tree a -> b
foldTree f g k t
   | null (subForest t) = f k t
   | otherwise          = g (map (foldTree f g k) (subForest t)) t

-- | @tupleMax@ returns the larger of two elements in a tuple
tupleMax :: (Ord a) => (a, a) -> a
tupleMax (a, b) = max a b

-- | @combineGLs@ is an unsuccessful attempt at creating a recursible function
--   for folding a tree to get a guestlist with most fun
combineGLs :: Employee -> [GuestList] -> (GuestList, GuestList)
combineGLs e gls = (a, b)
   where a = maximum ( map (glCons e) gls )
         b = maximum ( gls )

-- | @emptyGL@ is a utility variable
emptyGL :: GuestList
emptyGL = (GL [] 0)

-- | @nextLevel@ is a successful attempt at creating a recursible function
--   for finding a guestlist with most fun, given a tree of employees
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = ((glCons e emptyGL), emptyGL)
nextLevel e glps = (a, b)
   where a = glCons e ( foldr1 mappend (snd (unzip glps)) )
         b = ( foldr1 mappend (fst (unzip glps)) )

-- | @leafFunction@ is a function that can be applied for leaf nodes
--   during tree fold for purpose of getting optimal guestlist
leafFunction :: (GuestList, GuestList) -> Tree Employee -> (GuestList, GuestList)
leafFunction k t = nextLevel (rootLabel t) (k:[])

-- | @nodeFunction@ is a function that can be applied for non-leaf nodes
--   during tree fold for purpose of getting optimal guestlist
nodeFunction :: [(GuestList, GuestList)] -> Tree Employee -> (GuestList, GuestList)
nodeFunction leafs t = nextLevel (rootLabel t) leafs

-- | @maxFun t@ is a function to find optimal guestlist from a tree @t@
maxFun :: Tree Employee -> GuestList
maxFun t = tupleMax (foldTree leafFunction nodeFunction (emptyGL, emptyGL) t)

-- | @showGL@ is a utility function to convert a guestlist into its string
--   representation
showGL :: GuestList -> String
showGL (GL es fun) = "Total fun: " ++ (show fun) ++ "\n" ++ unlines (sort (map empName es))

-- | @main@ is the only IO function in this module. It reads a file containing
--   a company's employee tree and prints an optimal guestlist for it
main :: IO ()
main = do c <- readFile "company.txt"
          putStrLn (showGL (maxFun (read c)))
