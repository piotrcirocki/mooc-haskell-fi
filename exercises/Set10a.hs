module Set10a where

import Data.Char
import Data.List
import Data.List (elemIndex)
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Given a list, produce a new list where each element of the
-- original list repeats twice.
--
-- Make sure your function works with infinite lists.
--
-- Examples:
--   doublify [7,1,6]          ==>  [7,7,1,1,6,6]
--   take 10 (doublify [0..])  ==>  [0,0,1,1,2,2,3,3,4,4]

doublify :: [a] -> [a]
doublify x = zip x x >>= \(x,y) -> [x,y]

------------------------------------------------------------------------------
-- Ex 2: Implement the function interleave that takes two lists and
-- produces a new list that takes elements alternatingly from both
-- lists like this:
--
--   interleave [1,2,3] [4,5,6] ==> [1,4,2,5,3,6]
--
-- If one list runs out of elements before the other, just keep adding
-- elements from the other list.
--
-- Make sure your function also works with infinite lists.
--
-- Examples:
--   interleave [1,2,3] [4,5,6]            ==> [1,4,2,5,3,6]
--   interleave [1,2] [4,5,6,7]            ==> [1,4,2,5,6,7]
--   take 10 (interleave [7,7,7] [1..])    ==> [7,1,7,2,7,3,4,5,6,7]
--   take 10 (interleave [1..] (repeat 0)) ==> [1,0,2,0,3,0,4,0,5,0]

interleave :: (Eq a) => [a] -> [a] -> [a]
interleave x y = (zip x y >>= \(a,b) -> [a,b]) ++ (getChunk x y)

getChunk :: [a] -> [a] -> [a]
getChunk x y
  | length x > length y = drop (length y) x
  | length x < length y = drop (length x) y
  | otherwise = []

------------------------------------------------------------------------------
-- Ex 3: Deal out cards. Given a list of players (strings), and a list
-- of cards (strings), deal out the cards to the players in a cycle.
--  
-- Make sure your function works with infinite inputs as well!
--
--  Examples:
--   deal ["Hercule","Ariadne"] ["Ace","Joker","Heart"]
--     ==> [("Ace","Hercule"),("Joker","Ariadne"),("Heart","Hercule")]
--   take 4 (deal ["a","b","c"] (map show [0..]))
--     ==> [("0","a"),("1","b"),("2","c"),("3","a")]
--   deal ("you":(repeat "me")) ["1","2","3","4"]
--     ==> [("1","you"),("2","me"),("3","me"),("4","me")]
--
-- Hint: remember the functions cycle and zip?

-- Input: take 10 (cycle [1,2,3])
-- Output: [1,2,3,1,2,3,1,2,3,1] 

-- Input: zip [1,2,3] [9,8,7]
-- Output: [(1,9),(2,8),(3,7)] 

deal :: [String] -> [String] -> [(String,String)]
deal x y = zip  y (cycle x)

------------------------------------------------------------------------------
-- Ex 4: Compute a running average. Go through a list of Doubles and
-- output a list of averages: the average of the first number, the
-- average of the first two numbers, the first three numbers, and so
-- on.
--
-- Make sure your function works with infinite inputs as well!
--
-- Examples:
--   averages [] ==> []
--   averages [3,2,1] ==> [3.0,2.5,2.0]
--   take 10 (averages [1..]) ==> [1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5]

averages :: [Double] -> [Double]
averages x = map (avHlp x) $ zip [1 ..] x

avHlp :: [Double] -> (Int, Double) -> Double
avHlp  arr (idx, val)  = sum (take idx arr) / fromIntegral idx

------------------------------------------------------------------------------
-- Ex 5: Given two lists, xs and ys, and an element z, generate an
-- infinite list that consists of
--
--  * the elements of xs
--  * z
--  * the elements of ys
--  * z
--  * the elements of xs
--  * ... and so on
--
-- Examples:
--   take 20 (alternate "abc" "def" ',') ==> "abc,def,abc,def,abc,"
--   take 10 (alternate [1,2] [3,4,5] 0) ==> [1,2,0,3,4,5,0,1,2,0]

alternate :: [a] -> [a] -> a -> [a]
alternate xs ys z = cycle $  xs ++ [z] ++ ys ++ [z]

------------------------------------------------------------------------------
-- Ex 6: Check if the length of a list is at least n. Make sure your
-- function works for infinite inputs.
--
-- Examples:
--   lengthAtLeast 2 [1,2,3] ==> True
--   lengthAtLeast 7 [1,2,3] ==> False
--   lengthAtLeast 10 [0..]  ==> True


lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast stopper arr  = lengthHelper arr 1 stopper 

lengthHelper [] idx 0 = True
lengthHelper [] idx stopper = False
lengthHelper [x] idx stopper = (idx >= stopper)
lengthHelper (x:xs) idx stopper = if idx < stopper then lengthHelper xs (idx +1) stopper  else True

------------------------------------------------------------------------------
-- Ex 7: The function chunks should take in a list, and a number n,
-- and return all sublists of length n of the original list. The
-- sublists should be in the order that they appear in the original
-- list. A sublist means a slice, that is, a list of elements
-- a,b,c,... that occur in the original list next to each other and in
-- the same order.
--
-- Make sure your function works with infinite inputs. The function
-- lengthAtLeast can help with this.
--
-- Examples:
--   chunks 2 [1,2,3,4] ==> [[1,2],[2,3],[3,4]]
--   take 4 (chunks 3 [0..]) ==> [[0,1,2],[1,2,3],[2,3,4],[3,4,5]]
-- averages :: [Double] -> [Double]
-- averages x = map (avHlp x) $ zip [1 ..] x

-- avHlp :: [Double] -> (Int, Double) -> Double
-- avHlp  arr (idx, val)  = sum (take idx arr) / fromIntegral idx

-- ghci> let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
-- ghci> drop 2 (take 5 arr)
-- [3,4,5]

chunks :: Int -> [a] -> [[a]]
chunks chunkPt arr =  filter (\x -> length x == chunkPt) $ map ( chunkHlp arr chunkPt ) $ zip [0..] arr
 
chunkHlp :: [a] -> Int -> (Int, a) -> [a]
chunkHlp arr chunk  (idx, val) = drop idx (take (chunk + idx) arr)


------------------------------------------------------------------------------
-- Ex 8: Define a newtype called IgnoreCase, that wraps a value of
-- type String. Define an `Eq` instance for IgnoreCase so that it
-- compares strings in a case-insensitive way.
--
-- To help the tests, also implement the function
--   ignorecase :: String -> IgnoreCase
--
-- Hint: remember Data.Char.toLower
--
-- Examples:
--   ignorecase "abC" == ignorecase "ABc"  ==>  True
--   ignorecase "acC" == ignorecase "ABc"  ==>  False
newtype IgnoreCase = IgnoreC String

instance Eq IgnoreCase where
  (==)  ::  IgnoreCase -> IgnoreCase -> Bool
  (==) (IgnoreC x) (IgnoreC y) = compare (map toLower x) (map toLower y) == EQ 
    
ignorecase x = IgnoreC x

------------------------------------------------------------------------------
-- Ex 9: Here's the Room type and some helper functions from the
-- course material. Define a cyclic Room structure like this:
--
--  * maze1 has the description "Maze"
--    * The direction "Left" goes to maze2
--    * "Right" goes to maze3
--  * maze2 has the description "Deeper in the maze"
--    * "Left" goes to maze3
--    * "Right" goes to maze1
--  * maze3 has the description "Elsewhere in the maze"
--    * "Left" goes to maze1
--    * "Right" goes to maze2
--
-- The variable maze should point to the room maze1.
--
-- Examples:
--   play maze ["Left","Left","Left"]
--      ==> ["Maze","Deeper in the maze","Elsewhere in the maze","Maze"]
--   play maze ["Right","Right","Right","Right"]
--      ==> ["Maze","Elsewhere in the maze","Deeper in the maze","Maze","Elsewhere in the maze"]
--   play maze ["Left","Left","Right"]
--      ==> ["Maze","Deeper in the maze","Elsewhere in the maze","Deeper in the maze"]

data Room = Room String [(String,Room)]

-- Do not modify describe, move or play. The tests will use the
-- original definitions of describe, move and play regardless of your
-- modifications.

describe :: Room -> String
describe (Room s _) = s

move :: Room -> String -> Maybe Room
move (Room _ directions) direction = lookup direction directions

play :: Room -> [String] -> [String]
play room [] = [describe room]
play room (d:ds) = case move room d of Nothing -> [describe room]
                                       Just r -> describe room : play r ds

maze :: Room
maze = maze1
  where maze1 = Room  "Maze" [("Left", maze2), ("Right", maze3)]
        maze2 = Room  "Deeper in the maze" [("Left", maze3), ("Right", maze1)]
        maze3 = Room  "Elsewhere in the maze" [("Left", maze1), ("Right", maze2)]
