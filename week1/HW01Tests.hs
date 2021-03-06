-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0), (111, 1)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0), (2, 0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, ns) = toRevDigits n == ns

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits [(1234, [4,3,2,1]), (54321, [1,2,3,4,5]), (0, []), (-17, [])] ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (as, es) = doubleEveryOther as == es

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther [([4,9,5,5], [4,18,5,10]), ([0,0],[0,0]), ([1,0], [1,0])]]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (as, e) = sumDigits as == e

ex4Tests :: [Test]
ex4Tests = [Test "sumDigits test" testSumDigits [([10,5,18,4], 19), ([1,1,1,10], 4), ([1,111], 4), ([-1, -1, 11], 2)]]

-- Exercise 5 -----------------------------------------
testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, eBool) = luhn n == eBool

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn [(5594589764218858, True), (1234567898765432, False)] ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, s, g, t, moves) = (hanoi n s g t) == moves

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi [(2, "a" :: Peg, "b" :: Peg, "c" :: Peg, [("a" :: Peg, "c" :: Peg), ("a" :: Peg, "b" :: Peg), ("c" :: Peg, "b" :: Peg)])]]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
