{-# OPTIONS_GHC -Wall #-}

-- Naive fibonacci - Only usable for very small n (<30)
naive_fib :: Integer -> Integer
naive_fib 0 = 0
naive_fib 1 = 1
naive_fib n = naive_fib (n-1) + naive_fib (n-2)

fibs1 :: [Integer]
fibs1 = map naive_fib [0..]

-- Memoized fibonacci - Never re-evaluate thunks
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-1) + memoized_fib (n-2)

fibs2 :: [Integer]
fibs2 = map memoized_fib [0..]

