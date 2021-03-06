module Lists where

import Utils
import Data.List
import System.Random
import Data.Function
import Data.Maybe
import qualified Data.Map as M

-- P1
last' :: [a] -> Maybe a
last' [] = Nothing
last' (x:[]) = Just x
last' (x:xs) = last' xs

-- P2
beforeLast :: [a] -> Maybe a
beforeLast [] = Nothing
beforeLast (x:x2:[]) = Just x
beforeLast (x:xs)  = beforeLast xs

-- P3
elementAt' :: Integer -> [a] -> Maybe a
elementAt' _ [] = Nothing
elementAt' 0 (x:xs) = Just x
elementAt' a (x:xs) = elementAt' (a - 1) xs

-- OR
elementAt'' :: Integer -> [a] -> Maybe a
elementAt'' n = fmap snd . find ((==n) . fst) . zip [0..]

-- P4
length' :: [a] -> Integer
length' []     = 0
length' (x:xs) = 1 + length' xs

-- From the wiki
-- length' = sum . map (\_ -> 1)
-- length' = foldr ((+) . const 1) 0

-- P5
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- From the wiki
-- reverse' = foldl (flip (:)) []

-- P6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = (==) <*> reverse'

-- P7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List list) = foldr ((++) . flatten') [] list

-- P8
-- Lmao this is so bad
compress :: Eq a => [a] -> [a]
compress [] = []
compress a = foldr (\x y -> if x == (head y) then y else x:y) [last a] a

-- elegant solution from the wiki
-- compress = fmap head . group

-- P9
pack :: Eq a => [a] -> [[a]]
-- There is also a span in prelude that returns a tuple instead of doing dropWhile and takeWhile
pack [] = []
pack (x:xs) = [x : takeWhile (==x) xs] ++  pack (dropWhile (==x) xs)

-- P10
encodeRLE :: Eq a => [a] -> [(Int, a)]
encodeRLE = fmap (\x -> (length x, head x)) . group

-- P11
data RLE a = Multiple Int a | Single a deriving (Show)
encodeRLE' ::  Eq a => [a] -> [RLE a]
encodeRLE' = fmap (\(num, elem) -> if num == 1 then Single elem else Multiple num elem) . encodeRLE

-- P12
-- From the wiki
decodeRLE :: [(Int, a)] -> [a]
decodeRLE = concatMap $ uncurry replicate

-- P13
encodeRLEDirect :: Eq a => [a] -> [(Int, a)]
encodeRLEDirect []     = []
encodeRLEDirect (a:[]) = [(1, a)]
encodeRLEDirect (a:as) = if f == a then (x+1, f):xs else (1, a):(x, f):xs
  where ((x, f):xs) = encodeRLEDirect as


-- P14/15
-- NOTE(ym): use concatMap more
-- From the wiki: repli = flip $ concatMap . replicate
repli :: Int -> [a] -> [a]
-- repli _ [] = []
-- repli n (a:as) = replicate n a ++ repli n as
repli n as = foldr ((++) . replicate n) [] as

-- P16
dropNth :: [a] -> Int -> [a]
dropNth []  _ = []
dropNth a n   = take (n - 1) a ++ (dropNth (drop n a) n)

-- P17
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ []     = ([], [])
splitAt' 1 (a:as) = ([a], as)
splitAt' n (a:as) = let (f, s) = splitAt' (n - 1) as in (a:f, s)

-- OR
splitAt'' :: Int -> [a] -> ([a], [a])
splitAt'' n a = (take n a, drop n a)

-- P18
slice :: Int -> Int -> [a] -> [a]
slice f l xs = take (l-f+1) (drop (f-1) xs)

-- P19
shift' :: Int -> [a] -> [a]
shift' n a = drop smth a ++ take smth a
  where smth = if  n < 0 then (length a) + n else n

-- From the wiki
-- (-2) `mod` 10 = 8, see mod vs rem
shift'' xs n = take len . drop (n `mod` len) . cycle $ xs where len = length xs

-- P20
-- Meh this works sanely, 2 is the third element not the second you bastards
-- You can also use split at, take and drop and etc etc, many ways to do this one
removeAt' :: Int -> [a] -> (a, [a])
-- Should probably return maybe or something
-- removeAt' _ [] = (idk, [])
removeAt' 0 (a:as) = (a, as)
removeAt' n (a:as) = let (r, as) = removeAt' (n - 1) as in (r, a:as)

-- OR
removeAt'' :: Int -> [a] -> [a]
removeAt'' n as = snd . unzip . filter ((/=n) . fst) $ zip [0..length as] as

-- P21
-- Again, pretty easy, lots of solutions
insertAt' :: Int -> a -> [a] -> [a]
insertAt' _ _ []     = []
insertAt' 0 a xs     = a:xs
insertAt' n a (x:xs) = x:insertAt' (n - 1) a xs

-- P22
range' :: Int -> Int -> [Int]
-- lol
-- range f l = [f..l]
range' f l = helper f (l - f)
  where
    helper f 0 = f:[]
    helper f n = f:helper (f + 1) (n - 1)

-- From the wiki, iterate is pretty neat
range'' f l = take (f - l + 1) $ iterate (+1) f

-- P23
-- Super slow
rndSelect :: [a] -> Int -> IO [a]
rndSelect a n = (fmap . fmap) (a!!) $ newStdGen >>= return . take (min n la) . nub . randomRs (0, la - 1)
  where la = (length a)

-- P24
rndRange :: Int -> Int -> IO [Int]
rndRange n r = rndSelect [0..r] n

-- P25
rndPerm :: [a] -> IO [a]
rndPerm = rndSelect <*> length

-- P26
-- Binomial coefficients

-- P27

-- P28
-- Just plain quicksort
-- Unstable sort
sort' :: [[a]] -> [[a]]
sort' [] = []
sort' (a:as) = concat  $ [sort' $ filter ((< length a) . length) as, [a], sort' $ filter ((>= length a) . length) as]

fsortHelper = frequency >>= fsort

-- Not stable unlike the wiki
-- looks ugly af
fsort :: M.Map Int Int -> [[a]] -> [[a]]
fsort _ [] = []
fsort freq (a:as) = concat  $ [fsort freq $ filter (func (<)) as , [a], fsort freq $ filter (func (>=)) as]
  where func f x = fromMaybe False $ M.lookup (length x) freq >>= (\x -> M.lookup (length a) freq >>= (\y -> return $ f x y))

-- From the wiki, way smarter solution
fsort' = concat . sort' . groupBy ((==) `on` length) . sort'
