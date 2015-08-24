module Main where

-- -------------------------------------
-- Summation of primes
-- Problem 10
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
--
-- -------------------------------------
-- |
--
-- >>> answer_010 10
-- 17
--
-- -->>> answer_010 2000000
-- 142913828922
--
answer_010 :: Integer -> Integer
answer_010 max = sum (takeWhile (<max) prime_num_seq)

-- -------------------------------------
-- generate prime number sequence
-- -------------------------------------
-- |
--
-- >>> take 9 prime_num_seq
-- [2,3,5,7,11,13,17,19,23]
--
prime_num_seq :: [Integer]
prime_num_seq = f [2..] where
    f :: [Integer] -> [Integer]
    f [] = []
    f (p:candidates) = p : f (filter (\x -> x `mod` p /=0) candidates)

-- -------------------------------------
-- hello world sample
-- -------------------------------------
main :: IO ()
main = putStrLn "hello, world"

