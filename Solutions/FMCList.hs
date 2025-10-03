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
drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (_ : xs) = drop (n - 1) xs 

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x : xs) = if f x 
then
  x : takeWhile f xs
else
  []

  
-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x : xs) = if f x 
then
  dropWhile f xs
else
  x : xs


tails :: [a] -> [[a]]
tails [] = [[]]
tails [x] = [x] : [[]]
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a]
init [_] = []
init (x : xs) = x : init xs

-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x : ) (inits xs)

-- subsequences

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f [x] = f x
any f (x : xs) = f x || any f xs


-- all
all :: (a -> Bool) -> [a] -> Bool
all f [] = False
all f [x] = f x
all f (x : xs) = f x && all f xs



-- and
and :: [Bool] -> Bool
and [] = True
and [b] = b
and (b : bs) = b && and bs

-- or
or :: [Bool] -> Bool
or [] = True
or [b] = b
or (b : bs) = b || or bs


-- concat
--concat :: [a] -> [a] -> [a]
--concat [] y = y
--concat (x : xs) ys = x : concat xs ys
concat = (++)


-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem n xs = any (n == ) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x : xs) = n == x || elem' n xs


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

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x
-- replicate

last :: [a] -> a
last [] = error "Empty list"
last [x] = x
last (_ : xs) = last xs


-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys


-- isInfixOf | Not Done
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf _ [] = False
isInfixOf [] _ = True
isInfixOf xs (y : ys) = 
  case isPrefixOf xs (y : ys) of
    True  -> True
    False -> isInfixOf xs ys

-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)


-- zip
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys


-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys 

-- intercalate
intercalate :: [a] -> [[a]] -> [[a]]
intercalate _ [] = []
intercalate _ [y] = [y]
intercalate xs (y : ys) = y : xs : intercalate xs ys

-- nub
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : filter (/= x) (nub xs)


-- splitAt
splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 [] = ([], [])
splitAt _ [] = error "Index out of bounds"
splitAt n xs = if length xs == n
then
  (take n xs, drop n xs)
else
  (xs, [])

-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
--if n == length xs 
--   then crash ()
--   else normal ()

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break f xs = (takeWhile (not . f) xs, dropWhile (not . f) xs)


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

