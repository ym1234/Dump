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
xor' a b = and' (or' a b) (nand' a b)
-- Using lambda bot: (amazing lol)
-- ap (ap . (and' .) . or') nand'

-- Implies: https://en.wikipedia.org/wiki/Material_conditional
impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' = opp xor'


-- -- TODO(ym): Find a better name for this lol
-- ALSO WOW, replicateM and sequence are pretty amazing
combinations :: [a] -> [a]
combinations a = replicateM (length a) a
