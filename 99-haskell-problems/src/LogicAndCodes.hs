module LogicAndCodes where
import qualified Control.Monad as M
-- P46
opp :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool)
opp = ((not .) .)

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

nand' :: Bool -> Bool -> Bool
nand' = opp and'

nor' :: Bool -> Bool -> Bool
nor' = opp or'

xor' :: Bool -> Bool -> Bool
xor' a  b = and' (or' a b) (nand' a b)
-- Using lambda bot: (amazing lol)
-- ap (ap . (and' .) . or') nand'

-- Implies: https://en.wikipedia.org/wiki/Material_conditional
impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ'  :: Bool -> Bool -> Bool
equ' = opp xor'

-- replicateM and sequence are pretty amazing
allPermutations :: [a] -> [[a]]
allPermutations = length >>= M.replicateM

-- Copied from the wiki lol
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'` -- was 7, changing it to 3 got me the same results as in the original question :(

-- P46-48
table :: ([Bool] -> Bool) -> Int -> [[Bool]]
table a n = M.replicateM n [True, False] >>= (\f -> return $ f ++ [a f])

-- P49
-- https://en.wikipedia.org/wiki/Gray_code
-- Not doing memoization lol
greyCode :: Integer -> [String]
greyCode n = reverse $ helper n ["1", "0"]
  where helper 1 x = x
        helper n x = helper (n - 1) $ (('1':) <$> (reverse x)) ++ (('0':) <$> x)

-- better solution from the wiki
grayCode' :: Int -> [String]
grayCode' 0 = [""]
grayCode' n = let xs = grayCode' (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)
