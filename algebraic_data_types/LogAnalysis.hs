{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- | @parseMessage s@ parses a string @s@ containing a string form of log
--   message into a LogMessage object
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

-- | @getParam n d t s@ get a particular part number @n@ of a string @s@
--   by tokenizing based on delimiter @d@ and terminator @t@
getParam :: Int -> Char -> Char -> String -> String
getParam n d t s
   | n <= 1    = takeWhile (/= t) (lstrip d s)
   | otherwise = getParam ( n - 1 ) d t ( dropWhile (/= d) (lstrip d s) )

-- | @lstrip d s@ strips off all occurences of character @d@ from the left end
--   of a string @s@
lstrip :: Char -> String -> String
lstrip d s = dropWhile (== d) s

-- | @parse t@ breaks down given text @t@ into lines, prepares a
--   LogMessage for each of the lines, and returns a list of LogMessage
parse :: String -> [LogMessage]
parse t = map (parseMessage) ( lines t )

-- | @insert m t1 t2@ inserts a LogMessage @m@ into a message tree @t1@ to get
--   a new tree @t2@. (@t1@ and @t2@ are binary search trees)
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t -- prevent inserting an unknown into the tree
insert lm Leaf = Node Leaf lm Leaf
insert x@(LogMessage _ time _) (Node left root@(LogMessage _ rtime _) right)
   | time < rtime = Node (insert x left) root right   -- insert in left subtree
   | time > rtime = Node left root (insert x right)   -- insert in right subtree
   | otherwise    = (Node left x right)               -- don't insert

-- root is unknown - invalid scenario - should never occur
-- replace with a proper tree
insert x (Node _ (Unknown _) _) = (Node Leaf x Leaf) 

-- | @build lms@ builds a tree from a list of log messages
build :: [LogMessage] -> MessageTree
build lms = foldl (\t x -> insert x t) Leaf lms

-- | @inOrder t@ performs an in-order traversal of a message tree @t@ to get
--   a list of log messages sorter by key (timestamp)
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l x r) = (inOrder l) ++ [x] ++ (inOrder r)

-- | @sort@ performs a sorting of a list of log messages
sort :: [LogMessage] -> [LogMessage]
sort = inOrder.build

-- | @whatWentWrong lms@ composes a list of strings from a list of log messages
--   filtered based on a filter criterion
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = 
   [msg | Just (LogMessage _ _ msg) <- (map filterMessage lms)]

-- | @filterMessage lm@ performs a filter criterion check on a given log
--   message @lm@. Filter criterion: Error messages with severity @s@ > 50
filterMessage :: LogMessage -> Maybe LogMessage
filterMessage lm@(LogMessage (Error s) _ _) = 
   if s > 50 then (Just lm) else Nothing
filterMessage _ = Nothing
