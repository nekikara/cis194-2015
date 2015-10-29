{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `rem` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
    | n > 0  = (lastDigit n) : toRevDigits (dropLastDigit n)
    | otherwise = []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
    | odd (length xs) = x : (head xs) * 2 : doubleEveryOther (tail xs)
    | otherwise = x : doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ divideAllToSingleNum xs

divideAllToSingleNum :: [Integer] -> [Integer]
divideAllToSingleNum xs = concat $ map (\x -> toRevDigits x) xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (lastDigit . sumDigits . doubleEveryOther . toRevDigits $ n) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start goal temp | n == 1 = [(start, goal)]
                        | otherwise = (hanoi (n-1) start temp goal) ++ (hanoi 1 start goal temp) ++ (hanoi (n-1) temp goal start)
