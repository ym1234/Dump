module Arithmetic where

import Utils

-- P31
-- awful
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = check (ceiling $ sqrt $ fromIntegral n) 2
  where check f k
          | (div n k) * k == n && k /= n = False
          | k >= f = True
          | otherwise = check n (k + 1)

-- much better, from the internet
-- isPrime k = null [ x | x <- [2..k - 1], k `mod` x == 0]

-- P32
-- https://en.wikipedia.org/wiki/Euclidean_algorithm
gcd' :: Integral a => a -> a -> a
gcd' a b
  |  b == 0 = abs a
  |  otherwise = gcd' b (a `mod` b)

-- P33
coprime :: Integral a => a -> a -> Bool
coprime = ((==1) .) . gcd

-- P34
-- See wiki for faster solution
totient :: Integral a => a -> Int
totient 1 = 1
totient a = length . filter (coprime a) $ [1..a-1]

-- -- P35
-- primeFactors :: Integral a => a -> Int
-- primeFactors a = a
