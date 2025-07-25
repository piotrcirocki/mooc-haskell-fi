module Set12 where

import Data.Functor
import Data.Foldable
import Data.List
import Data.Monoid

import Mooc.Todo
import Control.Arrow (ArrowChoice(right))


------------------------------------------------------------------------------
-- Ex 1: Implement the function incrementAll that takes a functor
-- value containing numbers and increments each number inside by one.
--
-- Examples:
--   incrementAll [1,2,3]     ==>  [2,3,4]
--   incrementAll (Just 3.0)  ==>  Just 4.0

incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll = fmap (+1)

------------------------------------------------------------------------------
-- Ex 2: Sometimes one wants to fmap multiple levels deep. Implement
-- the functions fmap2 and fmap3 that map over nested functors.
--
-- Examples:
--   fmap2 on [[Int]]:
--     fmap2 negate [[1,2],[3]]
--       ==> [[-1,-2],[-3]]
--   fmap2 on [Maybe String]:
--     fmap2 head [Just "abcd",Nothing,Just "efgh"]
--       ==> [Just 'a',Nothing,Just 'e']
--   fmap3 on [[[Int]]]:
--     fmap3 negate [[[1,2],[3]],[[4],[5,6]]]
--       ==> [[[-1,-2],[-3]],[[-4],[-5,-6]]]
--   fmap3 on Maybe [Maybe Bool]
--     fmap3 not (Just [Just False, Nothing])
--       ==> Just [Just True,Nothing]

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = (fmap . fmap)

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = (fmap . fmap2)

------------------------------------------------------------------------------
-- Ex 3: below you'll find a type Result that works a bit like Maybe,
-- but there are two different types of "Nothings": one with and one
-- without an error description.
--
-- Implement the instance Functor Result

data Result a = MkResult a | NoResult | Failure String
  deriving Show

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f NoResult = NoResult
  fmap f (Failure a) = Failure a
  fmap f (MkResult a) = MkResult (f a)


-- instance Functor Maybe where
--   -- In this instance, the type of fmap is:
--   -- fmap :: (a -> b) -> Maybe a -> Maybe b
--   fmap f Nothing = Nothing
--   fmap f (Just x) = Just (f x)

-- instance Functor [] where
--   fmap = map

-- data Tree a = Leaf | Node a (Tree a) (Tree a)
-- instance Functor Tree where
--   fmap _ Leaf = Leaf
--   fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

-- instance Functor Pair where
--   -- fmap f applies f to all values
--   fmap f (Pair x y) = Pair (f x) (f y)

  -- instance Functor Logger where
  -- fmap f (Logger log x) = Logger log (f x)


------------------------------------------------------------------------------
-- Ex 4: Here's a reimplementation of the Haskell list type. You might
-- remember it from Set6. Implement the instance Functor List.
--
-- Example:
--   fmap (+2) (LNode 0 (LNode 1 (LNode 2 Empty)))
--     ==> LNode 2 (LNode 3 (LNode 4 Empty))

data List a = Empty | LNode a (List a)
  deriving Show

instance Functor List where
  fmap f Empty = Empty
  fmap f (LNode a c) = LNode (f a) (fmap f c)
------------------------------------------------------------------------------
-- Ex 5: Here's another list type. This time every node contains two
-- values, so it's a type for a list of pairs. Implement the instance
-- Functor TwoList.
--
-- Example:
--   fmap (+2) (TwoNode 0 1 (TwoNode 2 3 TwoEmpty))
--     ==> TwoNode 2 3 (TwoNode 4 5 TwoEmpty)

data TwoList a = TwoEmpty | TwoNode a a (TwoList a)
  deriving Show

instance Functor TwoList where
  fmap f TwoEmpty = TwoEmpty
  fmap f (TwoNode a b c) = TwoNode (f a) (f b) (fmap f c)

------------------------------------------------------------------------------
-- Ex 6: Count all occurrences of a given element inside a Foldable.
--
-- Hint: you might find some useful functions from Data.Foldable.
-- Check the docs! Or then you can just implement count directly.
--
-- Examples:
--   count True [True,False,True] ==> 2
--   count 'c' (Just 'c') ==> 1

count :: (Eq a, Foldable f) => a -> f a -> Int
count a = foldr (\x acc -> if x==a then acc + 1 else acc ) 0

------------------------------------------------------------------------------
-- Ex 7: Return all elements that are in two Foldables, as a list.
--
-- Examples:
--   inBoth "abcd" "fobar" ==> "ab"
--   inBoth [1,2] (Just 2) ==> [2]
--   inBoth Nothing [3]    ==> []

inBoth :: (Foldable f, Foldable g, Eq a) => f a -> g a -> [a]
inBoth xx = foldr (\x acc -> if x `elem` xx then acc ++ [x] else acc ) []


------------------------------------------------------------------------------
-- Ex 8: Implement the instance Foldable List.
--
-- Remember what the minimal complete definitions for Foldable were:
-- you should only need to implement one function.
--
-- After defining the instance, you'll be able to compute:
--   sum (LNode 1 (LNode 2 (LNode 3 Empty)))    ==> 6
--   length (LNode 1 (LNode 2 (LNode 3 Empty))) ==> 3

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr f b Empty =  b
  foldr f b (LNode a Empty) = f a b
  foldr f b (LNode a c) = f a (foldr f b c)

--   {-# LANGUAGE DeriveFoldable #-}
-- data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show, Foldable)

-- instance Foldable Tree where
--   foldMap f Empty = mempty
--   foldMap f (Leaf x) = f x
--   foldMap f (Node left x right) = foldMap f left <> f x <> foldMap f right

------------------------------------------------------------------------------
-- Ex 9: Implement the instance Foldable TwoList.
--
-- After defining the instance, you'll be able to compute:
--   sum (TwoNode 0 1 (TwoNode 2 3 TwoEmpty))    ==> 6
--   length (TwoNode 0 1 (TwoNode 2 3 TwoEmpty)) ==> 4

-- foldr :: (a -> b -> b) -> b -> t a -> b
-- instance Foldable Pair where
--   -- just like applying foldr over a list of length 2
--   foldr f initialValue (Pair x y) = f x (f y initialValue)

-- data TwoList a = TwoEmpty | TwoNode a a (TwoList a)
--   deriving Show

-- instance Functor TwoList where
--   fmap f TwoEmpty = TwoEmpty
--   fmap f (TwoNode a b c) = TwoNode (f a) (f b) (fmap f c)

instance Foldable TwoList where
  foldr :: (a -> b -> b) -> b -> TwoList a -> b
  foldr f init TwoEmpty = init
  foldr f init (TwoNode a b c) = (f a . f b) (foldr f init c)

------------------------------------------------------------------------------
-- Ex 10: (Tricky!) Fun a is a type that wraps a function Int -> a.
-- Implement a Functor instance for it.
--
-- Figuring out what the Functor instance should do is most of the
-- puzzle.

data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x -- in f we have here f: (Int -> a) in x: Int 

instance Functor Fun where
  --fmap :: (a -> b) -> Fun a -> Fun b
  --fmap :: (Int -> b) -> Fun a -> Fun b
  fmap :: (a -> b) -> Fun a -> Fun b
  fmap f (Fun x) =  Fun (f . x) -- x: (Int -> a)  f:(a->b)
  -- fmap c x

-- using fun c, change incoming a to b
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap (*2) [1,2,3]

------------------------------------------------------------------------------
-- Ex 11: (Tricky!) You'll find the binary tree type from Set 5b
-- below. We'll implement a `Foldable` instance for it!
--
-- Implementing `foldr` directly for the Tree type is complicated.
-- However, there is another method in Foldable we can define instead:
--
--   foldMap :: Monoid m => (a -> m) -> Tree a -> m
--
-- There's a default implementation for `foldr` in Foldable that uses
-- `foldMap`.
--
-- Instead of implementing `foldMap` directly, we can build it with
-- these functions:
--
--   fmap :: (a -> m) -> Tree a -> Tree m
--   sumTree :: Monoid m => Tree m -> m
--
-- So your task is to define a `Functor` instance and the `sumTree`
-- function.
--
-- Examples:
--   using the [] Monoid with the (++) operation:
--     sumTree Leaf :: [a]
--       ==> []
--     sumTree (Node [3,4,5] (Node [1,2] Leaf Leaf) (Node [6] Leaf Leaf))
--       ==> [1,2,3,4,5,6]
--   using the Sum Monoid
--     sumTree Leaf :: Sum Int
--       ==> Sum 0
--     sumTree (Node (Sum 3) (Node (Sum 2) Leaf Leaf) (Node (Sum 1) Leaf Leaf))
--       ==> Sum 6
--
-- Once you're done, foldr should operate like this:
--   foldr (:) [] Leaf   ==>   []
--   foldr (:) [] (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))  ==>   [1,2,3]
--
--   foldr (:) [] (Node 4 (Node 2 (Node 1 Leaf Leaf)
--                                (Node 3 Leaf Leaf))
--                        (Node 5 Leaf
--                                (Node 6 Leaf Leaf)))
--      ==> [1,2,3,4,5,6]
--
-- The last example more visually:
--
--        .4.
--       /   \
--      2     5     ====>  1 2 3 4 5 6
--     / \     \
--    1   3     6

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node x left right) =  Node (f x ) (fmap f left) (fmap f right)
  fmap f Leaf = Leaf

sumTree :: Monoid m => Tree m -> m
sumTree Leaf = mempty
sumTree (Node x left right) = sumTree left <> x <> sumTree  right

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f t = sumTree (fmap f t)

------------------------------------------------------------------------------
-- Bonus! If you enjoyed the two last exercises (not everybody will),
-- you'll like the `loeb` function:
--
--   https://github.com/quchen/articles/blob/master/loeb-moeb.md
