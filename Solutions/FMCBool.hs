module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show True = "True"
    show False = "False"

instance Enum Bool where

    toEnum  = undefined

    fromEnum  = undefined

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False 

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
True || _ = True
_ || True = True
_ || _ = False

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
True /|\ True = False
_ /|\ _ = True

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
True \|/ _ = False
_ \|/ True = False
_ \|/ _ = True

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True <=/=> True = False
False <=/=> False = False
_ <=/=> _ = True

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not a = a <=/=> True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b 

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) = undefined

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) = undefined

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) = undefined

infixr 1 <=>


