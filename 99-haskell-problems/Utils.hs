module Utils where

import qualified Data.Map as M
frequency :: [[a]] -> M.Map Int Int
frequency = foldr (M.alter (Just . maybe 1 (+1)) . length) M.empty

-- primes :: Int -> [[Int]]
-- primes n = helper [0..ceiling $ sqrt $ n] n
--   where helper x n =

-- Not actually a challenge lol
iterate'' :: (a -> a)  -> a -> [a]
iterate'' f x = x:iterate'' f (f x)

