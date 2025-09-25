module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
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

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero _ = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O
even (S O) = O
even (S (S n)) = even n 

odd :: Nat -> Nat
odd O = O
odd (S O) = S O
odd (S (S n)) = odd n 

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus n O = n
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
--(*) = undefined
_ * O = O
O * _ = O
S O * n = n
n * S O = n
n * (S x) = n * x + n


infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ (S m) = n ^ m * n 

-- decide: infix? ? ^

-- quotient

quotHelper :: Nat -> Nat -> Nat -> Nat -> Nat
quotHelper n m O (S s) =  s
quotHelper n m l s | m == l = quotHelper n m (l -* m) (S (S s))
quotHelper n m l s = quotHelper n m (l -* m) (S s)

(/) :: Nat -> Nat -> Nat
_ / O = undefined
n / S O = n
n / m = quotHelper n m n O  

-- remainder
(%) :: Nat -> Nat -> Nat
(%) _ O = undefined
(%) n m | (n / m) == O = O
(%) n m = n -* ((n / m) * m)

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) O O     = S O
(|||) _ O     = O
(|||) n m | ((m / n) * n) == m = S O
(|||) _ _ = O


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff O m = m
absDiff n O = n  
absDiff (S n) (S m) = absDiff n m

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = one
factorial (S n) = factorial n * S n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

--lo b a is the floor of the logarithm base b of a
loHelper :: Nat -> Nat -> Nat -> Nat -> Nat
loHelper n m (S q) O = 
  case (n ^ q) -* m of
    O -> q
    (S _) -> pred q
loHelper n m q _ = loHelper n m (S q) (m -* (n ^ q))


lo :: Nat -> Nat -> Nat
lo (S _) (S O) = O
lo O _ = undefined
lo _ O = undefined
lo (S O) (S (S _)) = undefined
lo n m = loHelper n m zero one 
