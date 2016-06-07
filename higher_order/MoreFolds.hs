{-# OPTIONS_GHC -Wall #-}

import Data.List

-- | @xor@ performs XOR logic operation on a list of booleans
xor :: [Bool] -> Bool
xor = foldr (\ x y -> not (x==y)) False 

-- | @map'@ is an alternate implementation of map using foldr
map' :: (a->b) -> [a] -> [b]
map' f = foldr (\ x y -> (f x) : y) []

-- | @sieveSundaram@ produces a list of odd primes upto 2n + 1
--   Refer https://en.wikipedia.org/wiki/Sieve_of_Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram = (map multiplier).filterer

-- | @filterer@ is a helper for sieveSundaram
filterer :: Integer -> [Integer]
filterer n = [1..n] \\ 
            [(i+j+2*i*j) | i<-[1..n], j<-[1..n], i<=j, j<=n, i+j+2*i*j <= n]

-- | @multiplier@ is a helper for sieveSundaram
multiplier :: Integer -> Integer
multiplier x = 2*x + 1
