-- This exercise set hides most of Prelude. You only have access to
-- the Bool, Int and list types, and pattern matching.
--
-- In particular, seq is not available, so you must use pattern
-- matching to force evaluation!

{-# LANGUAGE NoImplicitPrelude #-}

module Set10b where

import Mooc.VeryLimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Define the operator ||| that works like ||, but forces its
-- _right_ argument instead of the left one.
--
-- Examples:
--   False ||| False     ==> False
--   True ||| False      ==> True
--   undefined ||| True  ==> True
--   False ||| undefined ==> an error!

-- The call seq a b evaluates to b but forces a into WHNF.

-- Prelude> seq (not True) 3
-- 3
-- Prelude> seq undefined 3
-- *** Exception: Prelude.undefined
-- Prelude> (seq (not True) 3) + 7
-- 10
-- Prelude> (seq undefined 3) + 7
-- *** Exception: Prelude.undefined
-- Prelude> let f x = f x in seq (f 3) 3
-- -- ...infinite recursion

-- not :: Bool -> Bool
-- not True = False
-- not False = True

-- (||) :: Bool -> Bool -> Bool
-- True || _ = True
-- _    || x = x

-- even :: Int -> Bool
-- even x  =  (x == 0)  ||  not (even (x-1))

-- (|||) :: Bool -> Bool -> Bool
-- (|||) _ True = True 
-- (|||) True False = True
-- (|||) False False = False 

(|||) :: Bool -> Bool -> Bool
_ ||| True = True 
True ||| False = True
False ||| False = False 

------------------------------------------------------------------------------
-- Ex 2: Define the function boolLength, that returns the length of a
-- list of booleans and forces all of the elements
--
-- Examples:
--   boolLength [False,True,False] ==> 3
--   boolLength [False,undefined]  ==> an error!
--
-- Note that with the ordinary length function,
--   length [False,undefined] ==> 2

boolLength :: [Bool] -> Int
boolLength [] = 0 
boolLength [x] = boolCheck x + 1  
boolLength (x:xs) = 1 + boolCheck x + boolLength xs
--if  then y else y --if x == x then y else y
boolCheck :: Bool -> Int
boolCheck x = if x then 0 else 0

------------------------------------------------------------------------------
-- Ex 3: Define the function validate which, given a predicate and a
-- value, evaluates to the value. However, validate should also force the
-- result of `predicate value`, even though it is not used.
--
-- Examples:
--   validate even 3               ==>  3
--   validate odd 3                ==>  3
--   validate undefined 3          ==>  an error!
--   validate (\x -> undefined) 3  ==>  an error!

validate :: (a -> Bool) -> a -> a
validate predicate value = if predicate value then value else value

------------------------------------------------------------------------------
-- Ex 4: Even though we can't implement the generic seq function
-- ourselves, we can implement it manually for specific datatypes.
--
-- The type class MySeq contains the method myseq which is supposed to
-- work like the built-in seq function. Implement the given MySeq
-- instances.
--
-- Just like in the course material, we use the special value
-- `undefined` here to illustrate what myseq evaluates. The tests for
-- this exercise also use undefined.
--
-- Examples:
--   myseq True  0 ==> 0
--   myseq ((\x -> x) True) 0 ==> 0
--   myseq (undefined :: Bool) 0
--     ==> *** Exception: Prelude.undefined
--   myseq (3::Int) True ==> True
--   myseq (undefined::Int) True
--     ==> *** Exception: Prelude.undefined
--   myseq [1,2] 'z' ==> 'z'
--   myseq [undefined] 'z' ==> 'z'           -- [undefined] is in WHNF
--   myseq (1:undefined) 'z' ==> 'z'         -- 1:undefined is in WHNF
--   myseq (undefined:[2,3]) 'z' ==> 'z'     -- undefined:[2,3] is in WHNF
--   myseq [1..] 'z' ==> 'z'
--   myseq (undefined::[Int])
--     ==> *** Exception: Prelude.undefined

class MySeq a where
  myseq :: a -> b -> b
--isInt x = x == fromInteger (round x)
instance MySeq Bool where
  myseq x y = if x then y else y

instance MySeq Int where
  myseq 0 y = y
  myseq _ y = y

instance MySeq [a] where
  myseq (x:d) y = y
  myseq x y = y

