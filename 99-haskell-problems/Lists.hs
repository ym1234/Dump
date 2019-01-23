import Data.List

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
elementAt'' n a = fmap snd . find ((==n) . fst) $ zip [0..] a

-- P4
length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

-- From the wiki
-- length' = sum . map (\_ -> 1)
-- length' = foldr ((+) . const 1) 0

-- P5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- From the wiki
-- reverse' = foldl (flip (:)) []

-- P6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse' x

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
repli n as = foldr ((++) . replicate n) [] as

-- P16
dropNth :: [a] -> Int -> [a]
dropNth []  _ = []
dropNth a n = take (n - 1) a ++ (dropNth (drop n a) n)


