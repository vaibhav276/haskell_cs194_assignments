{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s
   | head s == 'I'   = 
      LogMessage Info 
         (read (getParam 2 ' ' ' ' s)) 
         (getParam 3 ' ' '\n' s)
   | head s == 'W'   =
      LogMessage Warning 
         (read (getParam 2 ' ' ' ' s)) 
         (getParam 3 ' ' '\n' s)
   | head s == 'E'   =
      LogMessage ( Error (read (getParam 2 ' ' ' ' s)) )
         (read (getParam 3 ' ' ' ' s)) 
         (getParam 4 ' ' '\n' s) 
   | otherwise       =
      Unknown (getParam 1 ' ' '\n' s)

getParam :: Int -> Char -> Char -> String -> String
getParam n d t s
   | n <= 1    = takeWhile (/= t) ( lstrip d s)
   | otherwise = 
      getParam ( n - 1 ) d t ( dropWhile (/= d) (lstrip d s) )

lstrip :: Char -> String -> String
lstrip d s = dropWhile (== d) s

-- | @parse t@ breaks down given text @t@ into lines, prepares a
--   LogMessage for each of the lines, and returns a list of LogMessage
parse :: String -> [LogMessage]
parse t = do
   map (parseMessage) ( lines t )


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t -- prevent inserting an unknown into the tree
insert lm Leaf = Node Leaf lm Leaf
insert x@(LogMessage _ time _) (Node left root@(LogMessage _ rtime _) right)
   | time < rtime = Node (insert x left) root right   -- insert in left subtree
   | time > rtime = Node left root (insert x right)   -- insert in right subtree
   | otherwise    = (Node left x right)               -- don't insert

-- root is unknown - invalid scenario - should never occur
insert x (Node _ (Unknown _) _)
   = (Node Leaf x Leaf) -- replace with a proper tree

build :: [LogMessage] -> MessageTree
build lms = foldl (\t x -> insert x t) Leaf lms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l x r) = (inOrder l) ++ [x] ++ (inOrder r)

sort :: [LogMessage] -> [LogMessage]
sort = inOrder.build

