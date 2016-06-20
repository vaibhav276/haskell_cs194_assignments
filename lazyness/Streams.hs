{-# OPTIONS_GHC -Wall #-}

-- Stream only allows infinite lists
data Stream a = Cons a (Stream a)

-- | @streamToList@ converts a stream to a list
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

-- utility override
instance Show a => Show (Stream a) where
   show = show.(take 20).streamToList

-- | @streamRepeat@ prepares a stream from an integer by infinite repeatition
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- | @streamMap@ is map function for streams
streamMap :: (a->b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- | @streamFromSeed@ prepares a stream from a seed integer by applying
--   a function repeatidly on result of last
streamFromSeed :: (a->a) -> a -> Stream a
streamFromSeed g x = Cons (x) (streamFromSeed g (g x))

-- nats is an insteace of stream with all natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- | @largest2@ gives the larget power of two that perfectly divides a given
--   natural number
largest2 :: Integer -> Integer
largest2 x 
   | x <= 0  = 0
   | (odd x) = 0
   | otherwise = 1 + largest2 (x `div` 2)

-- | @ruler@ creates a ruler by applying largest2 on stream of natutal numbers
ruler :: Stream Integer
ruler = streamMap largest2 nats
