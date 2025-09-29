{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    O == O = True
    (S n) == (S m) = n == m
    _ == _ = False 

instance Ord Nat where

    O <= _ = True
    (S n) <= (S m) = n <= m
    _ <= _ = False 

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O (S _) = O
    min (S _) O = O 
    min (S n) (S m) = S (min n m)
    min O O = O

    max O (S m) = S m
    max (S n) O = S n
    max (S n) (S m) = S (max n m)
    max O O = O


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False 

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n  

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n 



----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
O <+> m = m
n <+> O = n
(S n) <+> m = S (n <+> m)


-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O m = O
monus n O = n
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times O _ = O
times _ O = O
times (S O) m = m
times n (S O) = n
times (S n) m = times n m + m

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _ O = S O
pow n (S m) = pow n m * n

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = exp

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "Division by zero"
O </> _ = O
n </> m = if m <= n 
    then
        S ((n -* m) </> m )
    else
        O

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> m = n -* ((n </> m) * m)

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (_, O) = error "Division by zero"
eucdiv (n, m) = (n </> m, n <%> m) 

-- divides
(<|>) :: Nat -> Nat -> Bool
O <|> O = True
n <|> m = isZero (m <%> n)  

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = undefined

(|-|) = dist

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    --(-) = (<->)
    (-) = monus
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

