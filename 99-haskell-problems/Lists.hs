import Data.List

-- P1
last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs

-- P2
beforeLast :: [a] -> a
beforeLast (x:x2:[]) = x
beforeLast (x:xs)  = beforeLast xs

-- P3
elementAt' :: Integer -> [a] -> Maybe a
elementAt' _ [] = Nothing
elementAt' 0 (x:xs) = Just x
elementAt' a (x:xs) = elementAt' (a - 1) xs

-- P4
length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

-- P5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- P6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse' x

-- P7
-- Haskell is statically typed, so lists are homogeneous
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List list) = foldr ((++) . flatten') [] list

-- P8
-- Lmao this is so bad
compress :: Eq a => [a] -> [a]
compress [] = []
compress a = foldr (\x y -> if x == (head y) then y else x:y) [last' a] a

-- elegant solution from the wiki
-- compress = map head . group

-- P9
pack :: Eq a => [a] -> [[a]]
-- There is also a span in prelude that returns a tuple instead of doing dropWhile and takeWhile
pack [] = []
pack (x:xs) = [x : takeWhile (==x) xs] ++  pack (dropWhile (==x) xs)

-- P10
encodeRLE :: Eq a => [a] -> [(Int, a)]
encodeRLE = map (\x -> (length x, head x)) . group

-- P11
data RLE a = Multiple Int a | Single a deriving (Show)
encodeRLE' ::  Eq a => [a] -> [RLE a]
encodeRLE' = map (\(num, elem) -> if num == 1 then Single elem else Multiple num elem) . encodeRLE

-- P12
-- From the wiki
decodeRLE :: [(Int, a)] -> [a]
decodeRLE = concatMap (uncurry replicate)

-- P13
encodeRLEDirect :: Eq a => [a] -> [(Int, a)]
encodeRLEDirect [] = []
encodeRLEDirect (a:[])= [(1, a)]
encodeRLEDirect (a:as) = if f == a then (x+1, f):xs else (1, a):(x, f):xs
  where ((x, f):xs) = encodeRLEDirect as


-- P14/15
-- NOTE(ym): use concatMap more
-- From the wiki: repli = flip $ concatMap . replicate
repli :: Int -> [a] -> [a]
-- repli _ [] = []
-- repli n (a:as) = replicate n a ++ repli n as
repli n as = foldr (\x y -> replicate n x ++ y) [] as

-- P16
dropNth :: [a] -> Int -> [a]
dropNth []  _ = []
dropNth a n = take (n - 1) a ++ (dropNth (drop n a) n)


