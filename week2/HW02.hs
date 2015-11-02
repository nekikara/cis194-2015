{-# OPTIONS_GHC -Wall #-}
module HW02 where
import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

countMatch :: Int -> (Peg, Peg) -> Int
countMatch acc test
    | fst test == snd test = acc + 1
    | otherwise = acc

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches breaker maker = foldl (\acc test -> if fst test == snd test then acc + 1 else acc) 0 (zip breaker maker)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countUp :: [Int] -> Peg -> [Int]
countUp acc peg
    | peg == Red = [(acc !! 0) + 1] ++ tail acc
    | peg == Green = [head acc, (acc !! 1) + 1, acc !! 2, acc !! 3, acc !! 4, acc !! 5]
    | peg == Blue = [(acc !! 0), (acc !! 1), (acc !! 2) + 1, acc !! 3, acc !! 4, acc !! 5]
    | peg == Yellow = [(acc !! 0), (acc !! 1), (acc !! 2), (acc !! 3) + 1, acc !! 4, acc !! 5]
    | peg == Orange = [(acc !! 0), (acc !! 1), (acc !! 2), (acc !! 3), (acc !! 4) + 1, acc !! 5]
    | peg == Purple = [(acc !! 0), (acc !! 1), (acc !! 2), (acc !! 3), (acc !! 4), (acc !! 5) + 1]

countColors :: Code -> [Int]
countColors = foldl countUp [0, 0, 0, 0, 0, 0]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches breaker maker = foldl (\acc result -> acc + (min (fst result) (snd result))) 0 $ zip (countColors breaker) (countColors maker)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = let exact = exactMatches secret guess
                           noexact = (matches secret guess) - exact
                       in Move guess exact noexact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move guess _ _) secret = getMove secret guess == move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
