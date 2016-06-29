{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Data.Monoid
import Sized

-- | @JoinList is an efficient way to store lists
--   by expressing every list as a combination of sublists
data JoinList m a = Empty
   | Single m a
   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- | @tag extracts the annotation from top node in a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- | @(+++) appends two JoinLists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append ((tag l) <> (tag r)) l r

-- | @(!!?) is a slow way to safely get @ith element from a JoinList
--   O(n)
(!!?) :: [a] -> Int -> Maybe a
[]      !!?    _        = Nothing
_       !!?    i | i<0  = Nothing
(x:_)   !!?    0        = Just x
(_:xs)  !!?    i        = xs !!? (i-1)

-- | @jlToList converts a JoinList to a traditional list
jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a)   = [a]
jlToList (Append _ a b) = (jlToList a) ++ (jlToList b)

-- | @indexJ is a fast way to safely get ith element from a JoinList
--   O(log n)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ i (Single _ a) 
   | i == 1             = Just a
   | otherwise          = Nothing
indexJ i (Append _ l r)
   | getSize (size (tag l)) <= (i-1) = 
      indexJ (i - getSize (size (tag l)) - 1) r
   | otherwise          = indexJ i l

-- | @dropJ is a fast way to safely drop i elements from a JoinList
--   O(log n)
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty           = Empty
dropJ i l@(Single _ _) 
   | i > 0              = Empty
   | otherwise          = l
dropJ i (Append _ l r)
   | getSize (size (tag l)) <= i =
      dropJ (i - getSize (size (tag l))) r
   | otherwise          = dropJ i l

-- | @takeJ is a fast way to safely take i elements from a JoinList
--   O(log n)
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty           = Empty
takeJ i l@(Single _ _)
   | i == 1             = l
   | otherwise          = Empty
takeJ i (Append _ l r)
   | getSize (size (tag l)) <= i =
      takeJ (i - getSize (size (tag l))) r
   | otherwise          = takeJ i l
