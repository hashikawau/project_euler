#! /bin/env runghc

main :: IO ()
-- main = putStrLn $ show $ divisors 28
-- main = putStrLn $ show $ let lst = divisors 76576500 in (length lst, lst)
-- main = putStrLn $ show $ length $ take 1000000 triangleNumberSequence
-- main = putStrLn $ show $ length $ takeWhile (<=76576500) triangleNumberSequence
-- main = putStrLn $ show $ length $ triangleNumberSequenceTo 1000000
-- main = putStrLn $ show $ highlyDivisibeTringularNumber 5
-- main = putStrLn $ show $ highlyDivisibeTringularNumber 100
-- main = putStrLn $ show $ highlyDivisibeTringularNumber 150
-- main = putStrLn $ show $ highlyDivisibeTringularNumber 200
main = putStrLn $ show $ highlyDivisibeTringularNumber 500

highlyDivisibeTringularNumber :: Integer -> Integer
highlyDivisibeTringularNumber n = takeFirstIf (\x -> numDivisors x > n) (triangleNumberSequenceTo 100000)
-- highlyDivisibeTringularNumber n = takeFirstIf (\x -> numDivisors x > n) triangleNumberSequence

takeFirstIf :: (a -> Bool) -> [a] -> a
takeFirstIf pred xs = head $ filter pred xs

numDivisors :: Integer -> Integer
numDivisors n = 2 * (fromIntegral $ length $ divisors n)

divisors :: Integer -> [Integer]
divisors n = filter isFactor [1..(floor (sqrt (fromIntegral n)))]
    where isFactor x = n `mod` x == 0

triangleNumberSequenceTo :: Integer -> [Integer]
triangleNumberSequenceTo n
    | n < 1     = []
    | n == 1    = [1]
    | otherwise = reverse $ triangleNumberSequenceToHelper n 2 [1]

triangleNumberSequenceToHelper :: Integer -> Integer -> [Integer] -> [Integer]
triangleNumberSequenceToHelper n i xs@(x:_)
    | n < i     = xs
    | otherwise = triangleNumberSequenceToHelper n (i + 1) ((i + x) : xs)

triangleNumberSequence :: [Integer]
triangleNumberSequence = map nthTriangleNumber [1..]

nthTriangleNumber :: Integer -> Integer
nthTriangleNumber n = n * (n+1) `div` 2
-- nthTriangleNumber 1 = 1
-- nthTriangleNumber n = fromIntegral n + nthTriangleNumber (n - 1)




