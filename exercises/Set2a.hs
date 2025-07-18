-- Exercise set 2:
--  * Guards
--  * Lists
--  * Maybe
--  * Either
--
-- Functions you will need:
--  * head, tail - head returns the first element, tails everythin except the first 
--  * take, drop
--  * length
--  * null
--  * last
--  * init - everything except the last element
-- Frost Outpost
module Set2a where

import Mooc.Todo
--import Data.Ix (inRange)

-- Some imports you'll need. Don't add other imports :)
import Data.List
import Distribution.Compat.CharParsing (CharParsing(string))

------------------------------------------------------------------------------
-- Ex 1: Define the constant years, that is a list of the values 1982,
-- 2004 and 2020 in this order.

years = [1982, 2004, 2020]

------------------------------------------------------------------------------
-- Ex 2: define the function takeFinal, which returns the n last
-- elements of the given list.
--
-- If the list is shorter than n, return all elements.
--
-- Hint! remember the take and drop functions.

takeFinal :: Int -> [a] -> [a]
takeFinal n xs = if length xs < n
                    then xs
                    else drop ((length xs) - n) xs
------------------------------------------------------------------------------
-- Ex 3: Update an element at a certain index in a list. More
-- precisely, return a list that is identical to the given list except
-- the element at index i is x.
--
-- Note! indexing starts from 0
--
-- Examples:
--   updateAt 0 4 [1,2,3]   ==>  [4,2,3]
--   updateAt 2 0 [4,5,6,7] ==>  [4,5,0,7]

updateAt :: Int -> a -> [a] -> [a]
updateAt i x xs = case length xs of
                   0 -> []
                   1 -> [x]
                   _-> (take i xs) ++ [x] ++ (drop (i+1) xs) 

------------------------------------------------------------------------------
-- Ex 4: substring i j s should return the substring of s starting at
-- index i and ending at (right before) index j. Indexes start from 0.
--
-- Remember that strings are lists!
--
-- Examples:
--   substring 2 5 "abcdefgh"  ==>  "cde"
--   substring 2 2 "abcdefgh"  ==>  ""
--   substring 0 4 "abcdefgh"  ==>  "abcd"

substring :: Int -> Int -> String -> String
substring i j s = drop i (take j s) 

------------------------------------------------------------------------------
-- Ex 5: check if a string is a palindrome. A palindrome is a string
-- that is the same when read backwards.
--
-- Hint! There's a really simple solution to this. Don't overthink it!
--
-- Examples:
--   isPalindrome ""         ==>  True
--   isPalindrome "ABBA"     ==>  True
--   isPalindrome "racecar"  ==>  True
--   isPalindrome "AB"       ==>  False

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

------------------------------------------------------------------------------
-- Ex 6: implement the function palindromify that chops a character
-- off the front _and_ back of a string until the result is a
-- palindrome.
--
-- Examples:
--   palindromify "ab" ==> ""
--   palindromify "aaay" ==> "aa"
--   palindromify "xabbay" ==> "abba"
--   palindromify "abracacabra" ==> "acaca"

isDivBy2 :: String -> Bool
isDivBy2 s = length s `mod` 2 == 0

takeFirstPart :: String -> String
takeFirstPart s = take (length s `div` 2) s

takeSecondPartRev :: String -> String
takeSecondPartRev s
  | isDivBy2 s = reverse $ drop (length s `div` 2) s
  | otherwise = reverse $ drop ((length s `div` 2) + 1) s

getLettersHelper :: Int -> String -> String -> String -> String
getLettersHelper (-1) _ _ acc = acc
getLettersHelper idx str1 str2 acc
  | str1 !! idx == str2 !! idx = getLettersHelper (idx - 1) str1 str2 (acc ++ [str1 !! idx])
  | otherwise = acc

takePalindromFrom :: String -> String
takePalindromFrom s = getLettersHelper (length (takeFirstPart s) - 1) (takeFirstPart s) (takeSecondPartRev s) []

getMiddleLetter :: String -> Char
getMiddleLetter s = s !! (length s `div` 2)

pal :: String -> String
pal s
  | isDivBy2 s = let palPart = takePalindromFrom s in reverse palPart ++ palPart
  | otherwise = let palPart = takePalindromFrom s in reverse palPart ++ [getMiddleLetter s] ++ palPart

palindromify :: String -> String
palindromify s
  | null s = ""
  | length s == 1 = s
  | otherwise = pal s

------------------------------------------------------------------------------
-- Ex 7: implement safe integer division, that is, a function that
-- returns a Just result normally, but Nothing if the divisor is zero.
--
-- Remember that integer division can be done with the div function.
--
-- Examples:
--   safeDiv 4 2  ==> Just 2
--   safeDiv 4 0  ==> Nothing

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x y = case y of
                0 -> Nothing
                _ -> Just $ x `div` y
------------------------------------------------------------------------------
-- Ex 8: implement a function greet that greets a person given a first
-- name and possibly a last name. The last name is represented as a
-- Maybe String value.
--
-- Examples:
--   greet "John" Nothing         ==> "Hello, John!"
--   greet "John" (Just "Smith")  ==> "Hello, John Smith!"

greet :: String -> Maybe String -> String
greet first last = case last of
                     Nothing -> "Hello, " ++ first ++ "!"
                     Just last -> "Hello, " ++ first ++ " " ++ last ++ "!"
------------------------------------------------------------------------------
-- Ex 9: safe list indexing. Define a function safeIndex so that
--   safeIndex xs i
-- gets the element at index i in the list xs. If i is not a valid
-- index, Nothing is returned.
--
-- Examples:
--   safeIndex [True] 1            ==> Nothing
--   safeIndex [10,20,30] 0        ==> Just 10
--   safeIndex [10,20,30] 2        ==> Just 30
--   safeIndex [10,20,30] 3        ==> Nothing
--   safeIndex ["a","b","c"] (-1)  ==> Nothing

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i 
  | i >= 0 && i < length xs  = Just (xs !! i)
  | otherwise = Nothing

------------------------------------------------------------------------------
-- Ex 10: another variant of safe division. This time you should use
-- Either to return a string error message.
--
-- Examples:
--   eitherDiv 4 2   ==> Right 2
--   eitherDiv 4 0   ==> Left "4/0"

eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x y = case y of
                  0 -> Left (show x ++ "/" ++ show y)
                  _ -> Right (x `div` y)

------------------------------------------------------------------------------
-- Ex 11: implement the function addEithers, which combines two values of type
-- Either String Int into one like this:
--
-- - If both inputs were Ints, sum the Ints
-- - Otherwise, return the first argument that was not an Int
--
-- Hint! Remember pattern matching
--
-- Examples:
--   addEithers (Right 1) (Right 2) ==> Right 3
--   addEithers (Right 1) (Left "fail") ==> Left "fail"
--   addEithers (Left "boom") (Left "fail") ==> Left "boom"

addEithers :: Either String Int -> Either String Int -> Either String Int
addEithers (Right a)(Right b) = Right (a + b) --a b = todo
addEithers (Right a)(Left b) = Left b
addEithers (Left a)(Right b) = Left a
addEithers (Left a)(Left b) = Left a
