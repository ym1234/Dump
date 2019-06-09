module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

-- instance PopQuiz Bool
-- : Requires
-- instance PopQuiz [Bool]
-- : Requires
-- instance PopQuiz [a]
-- : Doesn't require
-- instance PopQuiz (a, b)
-- : Doesn't require
-- instance PopQuiz [(a, b)]
-- : Requires
-- instance PopQuiz (IO a)
-- Doesn't require

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/. YM: sneaky :P
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a)
-- : Requires
-- instance PopQuiz (RIO r a)
-- : Doesn't require
-- instance PopQuiz (RIO' r a)
-- : Requires TypeSynonymInstances
-- instance PopQuiz (r -> IO a)
-- : Requires
-- instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b).
-- : Doesn't require
-- instance PopQuiz (a -> b -> c)
-- : Requires
-- instance PopQuiz (a, b, c)
-- : Doesn't require
-- instance PopQuiz (a, (b, c))
-- : Requires
-- instance PopQuiz ()
-- : Doesn't require
-- instance PopQuiz (a, b, c, a)
-- : Doesn't require

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a)
-- : Requires
-- instance PopQuiz (Pair a)
-- : Doesn't require
-- instance PopQuiz (Pair' a)
-- : Requires TypeSynonymInstances
