
import Control.Monad

-- | Control Flow example
--
-- > :l App.hs
-- > ifStatement
-- > caseStatement
-- > guards


-- |
-- It's misleading to call 'if then else' control flow. They always
-- require an @else@. In general,
--  we are interested in constructing a value based on a Bool.
ifStatement :: Bool -> Int
ifStatement a = if a then 1 else 2


-- NO EQUAL SIGN
--
-- WRONG:
-- > gaurdStatement = undefined
--
gaurdStatement x y
  | x < 2     = "a"
  | x > 2     = "b"
  | otherwise = "c"


-- | Higher level control Flow
--
-- 1) How does looping map to other languages
--   * lists as iterators
--
-- 3) List Comprehension (pure)
-- > s = [ 2*x | x <- [0..], x^2 > 3 ]
--
-- 2) map, filter, fold
--
-- The ideas of fold, filter, map are ubiquitous in Haskell
--
-- @map@ is used
-- @filter@
-- @fold@
--
-- Remember, lists are used to represent nondeterministic calculations
-- and have Monad instance specifically for that.
--

whenStatement   cond = when   cond $ putStrLn "Yes"
unlessStatement cond = unless cond $ putStrLn "NO"
