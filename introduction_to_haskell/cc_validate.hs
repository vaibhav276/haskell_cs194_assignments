{-
Credit card number validation steps:
1. Convert card number to a string of integers
2. Reverse it
3. Double the digits at even locations
4. Calculate sum of individual digits
eg. [2,12,3,10,2,16] should be evaluated as 2+1+2+3+1+0+2+1+6
5. If sum is a multiple of 10, then the card number is valid
-}

-- Entry function
-- Input: 16 digit card number
-- Ouput: True or False indicating validity
validateCC :: Integer -> Bool
validateCC = validateCCImpl.toDigitsRev

-- Convert integer to a list (reverse order)
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
   | x <= 0    = []
   | x < 10    = [x]
   | otherwise = extractLSD x : toDigitsRev (x `div` 10)

-- Extract the least significant digit from a number
extractLSD :: Integer -> Integer
extractLSD x = x `mod` 10

-- Perform validation steps as mentioned at top
validateCCImpl :: [Integer] -> Bool
validateCCImpl xs = (mod (sumDigits(doubleEveryOther xs) ) 10) == 0

-- Double every even positioned number in a list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle[1,2])

-- Find sum of integers is a list (with each digit summed up internally)
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = innerSumDigits x + sumDigits xs

-- Find sum of all digits in an an Integer
innerSumDigits :: Integer -> Integer
innerSumDigits 0 = 0
innerSumDigits x = extractLSD x + innerSumDigits ( x `div` 10 )
