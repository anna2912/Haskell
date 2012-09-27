{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)

---------------------------------------------
-- ��������� ������-���������

-- ������������� �����������
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- ����� ������������� �����������
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- ����������� ���������
undefined = undefined

-- ���� ������� ����������� ��� �����, ��������� �� undefined ��������.
-- ����� ����� ����� ������������ (natEq � natLt --- ������� ���������).

-------------------------------------------
-- ����������� ����

-- ��� � ������������ ���������
data Unit = Unit deriving (Show,Read)

-- ����, ������������
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- �������, ��������������
data Either a b = Left a | Right b deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- ������� ��������, ��� ���������� if � ���� Bool ������������ ������,
-- ���� case ������ ��������.

-- �� ��� ����� ����������� ���� if
if' True a b = a
if' False a b = b

-- ����������. ������������� ���, ������������ ��������� ���������
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- ������ ��������

-- ���������� "��"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- ���������� "�"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- ���������� "���"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- ����������� �����

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- ���������� ��� ����������� �����
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ x) = LT
natCmp (Succ x) Zero = GT
natCmp (Succ x) (Succ y) = natCmp x y

-- n ��������� � m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n ������ m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- �������� ��� ����������� �����
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- ��������� ��� ����������� �����
(-.) :: Nat -> Nat -> Nat
n -. Zero = n 
Zero -. n = Zero
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- ��������� ��� ����������� �����
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- ����� � ������� �� ������� n �� m
natDivMod :: Nat -> Nat -> Pair Nat Nat 
natDivMod Zero n = Pair Zero Zero
natDivMod n Zero = error "division by 0"
natDivMod n m = if' (natLt m n) (Pair (natOne +. a) b) (Pair Zero n) where Pair a b = natDivMod (n -. m) m

natDiv n = fst . natDivMod n -- �����
natMod n = snd . natDivMod n -- �������

-- ����� GCD ���������� ������� (������ �������� 2 (���������������� �����) + 1 (���) �������)
gcd :: Nat -> Nat -> Nat
gcd a Zero = a
gcd a b = gcd b (natMod a b)

-------------------------------------------
-- ����� �����

-- ���������, ����� ������������� ������� ����� ���� ������������
data Int = Plus Nat | Minus Nat deriving (Show,Read)

intZero   = Plus Zero   -- 0
intOne    = Plus (Succ Zero)     -- 1
intNegOne = Minus Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Minus n) = Plus (Succ n)
intNeg (Plus (Succ n)) = Minus n
intNeg (Plus Zero) = Plus Zero

-- ������ ����� ��� ��� �����������
intCmp :: Int -> Int -> Tri
intCmp (Minus a) (Plus b) = LT
intCmp (Plus a) (Minus b) = GT
intCmp (Plus a) (Plus b) = natCmp a b
intCmp (Minus a) (Minus b) = natCmp b a

intEq :: Int -> Int -> Bool
intEq a b = case (intCmp a b) of 
				EQ -> True
				_ -> False

intLt :: Int -> Int -> Bool
intLt a b = case (intCmp a b) of 
				LT -> True
				_ -> False

infixl 6 .+., .-.
-- � ���� ��� ������������ �������� ���� �� ��� �����
(.+.) :: Int -> Int -> Int
(Plus Zero) .+. n = n
(Plus n) .+. (Plus m) = Plus (n +. m)
(Plus (Succ n)) .+. (Minus Zero) = Plus n
(Plus (Succ n)) .+. (Minus (Succ m)) = Plus n .+. Minus m
(Minus n) .+. (Minus m) = Minus (Succ n +. m)
(Minus n) .+. (Plus m) = Plus m .+. Minus n

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Plus Zero) .*. _ = Plus Zero
(Plus a) .*. (Plus b) = Plus (a *. b)
(Minus a) .*. (Minus b) = Plus ((Succ a) *. (Succ b))
(Plus a) .*. (Minus b) = Minus c where Succ c = a *. b
(Minus a) .*. (Plus b) = (Plus b) .*. (Minus a)

-------------------------------------------
-- ������������ �����

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- � ������������ ��� ���� �������� ��������
ratInv :: Rat -> Rat
ratInv (Rat _ Zero) = error "0 in denominator"
ratInv (Rat (Plus Zero) _) = error "can't invert 0"
ratInv (Rat (Plus (Succ a)) (Succ b)) = (Rat (Plus (Succ b)) (Succ a))
ratInv (Rat (Minus a) b) = Rat (Minus b) a

-- ������ ��� ������
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a Zero) (Rat c d) = error "0 in denominator"
ratCmp (Rat a b) (Rat c Zero) = error "0 in denominator"
ratCmp (Rat a (Succ b)) (Rat c (Succ d)) = intCmp (a .*. Plus(Succ d)) (c .*. Plus(Succ b))

ratEq :: Rat -> Rat -> Bool
ratEq a b = case (ratCmp a b) of 
				EQ -> True
				_ -> False

ratLt :: Rat -> Rat -> Bool
ratLt a b = case (ratCmp a b) of 
				LT -> True
				_ -> False

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a Zero) %+ (Rat c d) = error "0 in denominator"
(Rat a b) %+ (Rat c Zero) = error "0 in denominator"
(Rat a (Succ b)) %+ (Rat c (Succ d)) = Rat (a .*. Plus (Succ d) .+. c .*. Plus (Succ b)) (Succ b *. Succ d)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a Zero) %* (Rat c d) = error "0 in denominator"
(Rat a b) %* (Rat c Zero) = error "0 in denominator"
(Rat a (Succ b)) %* (Rat c (Succ d)) = Rat (a .*. c) (Succ b *. Succ d)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- �������� ��� ���������.
-- ���������� �����, �� ������������ ����� � ����

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- ������������� �����������
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- � ��� ������������� �����������
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
