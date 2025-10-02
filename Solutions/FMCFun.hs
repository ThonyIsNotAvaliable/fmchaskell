{-# LANGUAGE GADTs #-}

module ExList where

import Prelude hiding
    ( (.) , ($)
    , flip , curry , uncurry
    , iterate
    )


testCurry :: (Int, Int) -> Int
testCurry (a, b) = a + b 

testUncurry :: Int -> Int -> Int
testUncurry a b = a + b


testComp :: Int -> Int
testComp x = x * 2

testComp' :: Int -> Int
testComp' x = 10 - x 

-- use your mind to infer the types, don't cheat!

-- curry takes a "traditional" binary function
-- and returns its currified version
curry :: ((a, b) -> c) -> (a -> b -> c) 
curry f a b = f (a, b) 

-- uncurry takes a currified function
-- and returns its "traditional" binary version
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b
 

-- flip takes a (currified) binary function
-- and returns one that behaves the same but takes its arguments in the opposite orderi
flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

-- (.) takes two composable functions and returns their composition
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g a = f (g a)

-- f ° g (a) === f(g(a))

-- (.>) is composition but in diagramatic notation (should be ; but Haskell forbids)
(.>) = flip (.)

-- ($) takes a function and a suitable argument and applies the function to the argument
-- think: why would we ever want that?
($) :: (a -> b) -> a -> b
f $ a = f a

-- iterate: figure it out by its type
iterate :: (a -> a) -> a -> [a]
iterate f a = a : iterate f (f a)

-- orbit
orbit = flip iterate

