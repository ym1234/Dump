{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ExistentialQuantification #-}

module Exercises where

{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CNil :: CountableList
  CCons :: Countable a => a -> CountableList -> CountableList
  -- ...

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.
countList :: CountableList -> Int
countList CNil = 0
countList (CCons a l) = count a + countList l

-- | c. Write a function that removes all elements whose count is 0.
dropZero :: CountableList -> CountableList
dropZero CNil = CNil
dropZero (CCons a l) = if count a == 0 then dropZero l else CCons a (dropZero l)

-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?
-- Ans: Impossible
-- Because we don't know anything about the type of 'a'
filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"

{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.
data AnyList where
  ANil :: AnyList
  ACons :: a -> AnyList -> AnyList

aAppend :: AnyList -> AnyList -> AnyList
aAppend ANil a = a
aAppend b ANil = b
aAppend (ACons a b) c = (ACons a $ aAppend b c)

-- | b. How many of the following functions can we implement for an 'AnyList'?
-- Ans: 3, reverse, length, isempty
reverseAnyList :: AnyList -> AnyList
reverseAnyList ANil = ANil
reverseAnyList (ACons a l) = aAppend l  (ACons a ANil)

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

lengthAnyList :: AnyList -> Int
lengthAnyList ANil = 0
lengthAnyList (ACons _ l) =  1 + lengthAnyList l

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList ANil = True
isEmptyAnyList (ACons _ _) = False

-- nope, not even u
instance Show AnyList where
  show = error "What about me?"

{- THREE -}

-- | Consider the following GADT:
data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:
transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?
-- input is the existential type, apply the first param to it


-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?
instance Eq a => Eq (TransformableTo a) where
  (TransformWith a b) == (TransformWith c d) = a b == c d

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?
instance Functor TransformableTo where
  fmap f (TransformWith a b) = TransformWith f $ a b

{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?
pairEq :: EqPair -> Bool
pairEq (EqPair a b) = a == b

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)
-- Ans:
-- data EqPair a where
--   EqPair :: Eq a => a -> a -> EqPair a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?
-- Yes, we would, due to the constraint on a (type constraints are only allowed on functions withotu GADTs or other similar extensions)

{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox i _)) = i

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _ b) = 1 + countLayers b
countLayers (StringBox _ b) = 1 + countLayers b
countLayers (BoolBox _ b) = 1 + countLayers b


-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?
-- BoolBox -> StringBox
-- StringBox -> IntBox
-- IntBox -> EmptyBox
-- EmptyBox -> EmptyBox (or Nothing)
-- Removing one layer might return a different result

{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

head :: HList (h, t) -> h
head (HCons h _)  = h

tail :: HList (h, t) -> HList t -- notice how we can't do anything to HList after we tail,
tail (HCons _ t) = t

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?
-- None of them will work, HNil "returns" a Hlist (), and HCons returns a HList (head, tail)

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?
-- We cannot determine the type of the resulting list

{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

-- YM: I have no idea how to solve this lmao
data HTree a where
  -- ...
-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ALNil :: AlternatingList a b
  ALCons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ALNil  = []
getFirsts (ALCons a b) = a:getSeconds b

getSeconds :: AlternatingList a b -> [b]
getSeconds (ALCons a (ALCons b c)) = b:getSeconds c
getSeconds _ = []

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues x = (foldl1 (<>) (getFirsts x), foldl1 (<>) (getSeconds x))

foldValues' :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues' ALNil = (mempty, mempty)
foldValues' (ALCons a b) = (a <> j, k) where (k, j) = foldValues' b

{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:
eval :: Expr a -> a
eval (Equals a b) = eval a == eval b
eval (IntValue a) = a
eval (BoolValue a) = a
eval (Add a b) = eval a + eval b
eval (If a b c) = if eval a then eval b else eval c

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

-- little duplication hurts no one :P
parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyEquals a b) = do
  (IntValue x, IntValue y) <- (,) <$> parse a <*> parse b
  return $ IntValue $ if x == y then 1 else 0

parse (DirtyAdd a b) = do
  (IntValue x, IntValue y) <- (,) <$> parse a <*> parse b
  return $ IntValue $ x + y

parse (DirtyBoolValue x) = return $ IntValue $ if x then 1 else 0
parse (DirtyIntValue x) = return $ IntValue x
parse (DirtyIf a b c) = do
  (IntValue x) <- parse a
  y <- parse b
  z <- parse c
  return $ if x == 1 then y else z


-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?

{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TANil :: TypeAlignedList a a
  TACons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

-- data TypeAlignedList a c = TANil | forall b. TACons (a -> b) (TypeAlignedList b c) -- doesn't work lol, TNils is from a -> b but we need it to be a -> a (since there is no way to get a general "b")

-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

apply :: TypeAlignedList a b -> a -> b
apply TANil a = a
apply (TACons a b) c = apply b $ a c

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs a b = TACons (apply b) a

