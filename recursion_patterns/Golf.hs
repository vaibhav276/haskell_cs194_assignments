module Golf where

import Data.List

-------------------Task 1------------------
-- | @skips xs@ prepares a new list of lists, where each list is prepared by
--   taking every nth element from the list @xs@, where n ranges from 1 to 
--   length(xs)
skips :: [a] -> [[a]]
skips xs = [ getEvery n xs | n <- [1..(length xs)] ]
   
-- | @getEvery n xs@ returns a list of every @n@th element from the list @xs@
getEvery :: Int -> [a] -> [a]
getEvery n xs 
   | n > length xs = []
   | otherwise     = head (drop (n - 1) xs) : ( getEvery n ( drop n xs) )

-------------------Task 2------------------
-- | @localMaxima xs@ returns a list of local maximas in a list
--   A local maxima is an element which is larger than its adjescent elements
localMaxima :: [Integer] -> [Integer]
localMaxima xs 
   | length xs < 3 = []
   | otherwise     = 
   (if b > a && b > head c then [b] else []) ++ localMaxima (tail xs)
   where (a:b:c) = take 3 xs

-------------------Task 3------------------
-- | @histogram xs@ returns a putStr ready string containing the histogram of
--   given data (only numbers 0 to 9 are processed, others are ignored)
histogram :: [Integer] -> String
histogram xs = plot xs ++ "==========\n0123456789\n"

-- | @plot xs@ helps histogram function - prepares the plot part of the chart
plot :: [Integer] -> String
plot [] = ""
plot xs = plot ( xs \\ nub xs ) ++ plotLine (nub xs) ++ "\n" 

-- | @plotLine xs@ helps plot function - prepares the plot of a single line 
--   Input should contain distinct elements to work as expected
plotLine :: [Integer] -> String
plotLine xs = [(\x xs -> if (elem x xs) then '*' else ' ') x xs | x <- [0..9]]
