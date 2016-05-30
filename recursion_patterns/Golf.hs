module Golf where

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

-- | @localMaxima xs@ returns a list of local maximas in a list
--   A local maxima is an element which is larger than its adjescent elements
localMaxima :: [Integer] -> [Integer]
localMaxima xs 
   | length xs < 3 = []
   | otherwise     = 
   (if b > a && b > head c then [b] else []) ++ localMaxima (tail xs)
   where (a:b:c) = take 3 xs
