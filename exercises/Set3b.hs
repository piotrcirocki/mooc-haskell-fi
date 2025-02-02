-- Exercise set 3b
--
-- This is a special exercise set. The exercises are about
-- implementing list functions using recursion and pattern matching,
-- without using any standard library functions. For this reason,
-- you'll be working in a limited environment where almost none of the
-- standard library is available.
--
-- At least the following standard library functions are missing:
--  * (++)
--  * head
--  * tail
--  * map
--  * filter
--  * concat
--  * (!!)
--
-- The (:) operator is available, as is list literal syntax [a,b,c].
--
-- Feel free to use if-then-else, guards, and ordering functions (< and > etc.).
--
-- The tests will check that you haven't added imports :)

{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo
--import Data.ByteString (length)
--import Set3a (Points(xVal))
--import Data.ByteString (replicate)

------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
--
-- Use recursion and the : operator to build the list.
--
-- Examples:
--   buildList 1 5 2 ==> [1,1,1,1,1,2]
--   buildList 7 0 3 ==> [3]
iterate f 0 x = []
iterate f 1 x = [x]
iterate f n x = x : iterate f (n-1) (f x)

iterate1 0 x a = [a]
iterate1 n x a = x : iterate1 (n - 1) x a

buildList :: Int -> Int -> Int -> [Int]
buildList start count = iterate1 count start

------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Use recursion and the : operator to build the list.
--
-- Ps. you'll probably need a recursive helper function

sums :: Int -> [Int]
sums i = reverseList (iterate2 i)

-- iterateArr  0 x = []
-- iterateArr  1 x = [x]
-- iterateArr  n x = x : iterateArr  (n-1) x

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

iterate2 0 = []
iterate2 n = sumList (iterate (+1) n 1) : iterate2 (n-1)

-- iterate3 a n acc
--   | a <= n =  iterate3 (a+1) n (sumList ((iterate (+1) a 1)) :acc)
--   | a > n = acc 

reverseList = go []
    where
        go acc [] = acc
        go acc (x:xs) = go (x:acc) xs

------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def [] = def
mylast def [x] = x
mylast def (x:xs) = mylast def xs
--mylast def xs = todo

------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   indexDefault [True] 1 False         ==>  False
--   indexDefault [10,20,30] 0 7         ==>  10
--   indexDefault [10,20,30] 2 7         ==>  30
--   indexDefault [10,20,30] 3 7         ==>  7
--   indexDefault ["a","b","c"] (-1) "d" ==> "d"

indexDefault :: [a] -> Int -> a -> a
indexDefault xs i def = go xs i def 0

go [] i def acc = def
go (x:xs) i def acc = if acc == i then x else go xs i def (acc +1)



-- iterate3 a n acc
--   | a <= n =  iterate3 (a+1) n (sumList ((iterate (+1) a 1)) :acc)
--   | a > n = acc 

------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
--
-- Use pattern matching and recursion to iterate through the list.
--
-- Examples:
--   sorted [1,2,3] ==> True
--   sorted []      ==> True
--   sorted [2,7,7] ==> True
--   sorted [1,3,2] ==> False
--   sorted [7,2,7] ==> False

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:xs) = go1 xs x

go1 [] prev = True
go1 (x:xs) prev = (prev <= x) && go1 xs x

------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
--
-- Use pattern matching and recursion (and the list constructors : and [])


len :: [Int] -> Int
len [] = 0
len (x:xs) = 1 + len xs

iterateThrough 0 x acc  = []
iterateThrough n (x:xs) acc  = sumList (x:acc) : iterateThrough (n - 1) xs (x:acc)

sumsOf :: [Int] -> [Int]
sumsOf [] = []
sumsOf [x] = [x]
sumsOf xs = iterateThrough (len xs) xs []

------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x>y then y:merge (x:xs) ys else x:merge xs (y:ys)

------------------------------------------------------------------------------
-- Ex 8: compute the biggest element, using a comparison function
-- passed as an argument.
--
-- That is, implement the function mymaximum that takes
--
-- * a function `bigger` :: a -> a -> Bool
-- * a value `initial` of type a
-- * a list `xs` of values of type a
--
-- and returns the biggest value it sees, considering both `initial`
-- and all element in `xs`.
--
-- Examples:
--   mymaximum (>) 3 [] ==> 3
--   mymaximum (>) 0 [1,3,2] ==> 3
--   mymaximum (>) 4 [1,3,2] ==> 4    -- initial value was biggest
--   mymaximum (<) 4 [1,3,2] ==> 1    -- note changed biggerThan
--   mymaximum (\(a,b) (c,d) -> b > d) ("",0) [("Banana",7),("Mouse",8)]
--     ==> ("Mouse",8)

iterateOver 0 x = []
iterateOver n x = x : iterateOver (n - 1) x

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum f initial [] = initial
mymaximum f initial (x:xs) = if f initial x then mymaximum f initial xs else mymaximum f x xs

------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching. Do not use any library functions.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]

map2 f as bs = reverseList (go2 f as bs [])

go2 f [] [] arr = arr
go2 f as [] arr = arr
go2 f [] bs arr = arr

go2 f (a:as) (b:bs) arr =  go2 f as bs (f a b : arr) 

------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap f xs = todo
