{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String, Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

--Sugar for ease of testing
testAssist :: Num a => [a]
testAssist = [1, 2, 3, 4, 5, 6]


{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "Nil list"
head (x : _) = x

tail :: [a] -> [a]
tail [] = error "Nil list"
tail (_ : xs) = xs 

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (_ : xs) =  1 + length xs 

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs 

product :: Num a => [a] -> a
product [] = 0
product [x] = x
product (x : xs) = x * product xs

-- (snoc is cons written backwards)
-- Alteração no lugar para uso em reverse.

snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc xi (xl : xs) = xl : snoc xi xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = snoc x (reverse xs)


(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "Empty list"
minimum [x] = x
minimum [x, x'] = if x < x'
  then x
  else x'
minimum (x : xs) = minimum (x : [minimum  xs])


maximum :: Ord a => [a] -> a
maximum [] = error "Empty list"
maximum [x] = x

maximum [x, x'] = if x > x'
  then x
  else x'
maximum (x : xs) = maximum (x : [maximum  xs])


take :: Int -> [b] -> [b]
take _ [] = error "Index out of bounds"
take 0 _ = []
take i (x : xs) = x : take (i - 1) xs

-- drop

-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs) = case f x of
  True ->  x : filter f xs
  False -> filter f xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = []
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat x = x : repeat x
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose


-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome [] = True
palindrome xs = simplifyStr xs == reverse (simplifyStr xs)
  where
    simplifyStr :: String -> String
    simplifyStr [x] = [x]
    simplifyStr (x : xs) = if C.isAlpha x
    then
      C.toLower x : simplifyStr xs
    else
      simplifyStr xs


    
{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

