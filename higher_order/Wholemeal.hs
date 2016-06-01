-- The task is to refactor given functions into more Haskell idiomatic style

------------------------ (1) ----------------------------
-- Before:
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
   | even x = (x - 2) * fun1 xs
   | otherwise = fun1 xs

-- After:
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . (filter even)

------------------------ (2) ----------------------------
-- Before:
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2) 
       | otherwise = fun2 (3 * n + 1)

-- After
fun2' :: Integer -> Integer
fun2' = sum
         .filter even
         .takeWhile (>1)
         .iterate (\x -> if (even x) then (x `div` 2) else (3*x + 1))
