{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- ��� ���� ������?
--
-- ��� undefined ���������� � ��������� �����.
-- ��������� (*) �������� �����, � ������� ����� ������������� ������.

---------------------------------------------
-- �����������

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- ��������

-- ����� ������
length :: List a -> Nat
length Nil = Zero
length (Cons x xs) = Succ (length xs)

-- ������� ��� ������ �� O(length a)
(++) :: List a -> List a -> List a
Nil ++ x = x
(Cons x xs) ++ y = Cons x (xs ++ y)

-- ������ ��� ������� ��������
tail :: List a -> List a
tail Nil = error "empty list"
tail (Cons x xs) = xs

-- ������ ��� ���������� ��������
init :: List a -> List a
init Nil = error "empty list"
init (Cons x Nil) = Nil
init (Cons x xs) = Cons x (init xs)

-- ������ �������
head :: List a -> a
head Nil = error "empty list"
head (Cons x xs) = x

-- ��������� �������
last :: List a -> a
last Nil = error "empty list"
last (Cons x Nil) = x
last (Cons x xs) = last xs

-- n ������ ��������� ������
take :: Nat -> List a -> List a
take Zero xs = Nil
take (Succ n) Nil = Nil
take (Succ n) (Cons x xs) = Cons x (take n xs)

-- ������ ��� n ������ ���������
drop :: Nat -> List a -> List a
drop Zero xs = xs
drop (Succ n) Nil = Nil
drop (Succ n) (Cons x xs) = drop n xs

-- �������� � ������ ������ �������� ��������������� p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons x xs) = if' (p x) (Cons x (filter p xs)) (filter p xs)

-- ���������� ������. ������ "���������/��������" p
-- ������� "���������/�������� b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons x xs) = case p x of
		Just y -> Cons y (gfilter p xs)
		Nothing -> gfilter p xs

-- ���������� �� ������ � ��������� �� ������� ��������� ���������
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons x xs) = if' (p x) (Cons x (takeWhile p xs)) Nil

-- �� ���������� �� ������ � ��������� �� ������� ��������� ���������,
-- ����� ���� ����������� ��� ��������, ������� ������ ����������
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons x xs) = if' (p x) (dropWhile p xs) (Cons x xs)

-- ������� ������ �� ��������� �� (takeWhile p xs, dropWhile p xs),
-- �� �����������
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p = undefined

-- ������� ������ �� ��������� �� (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- �� �����������
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break = undefined

-- n-�� ������� ������ (������ � ����)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
l  !! n = undefined

-- ������ ����� �� ����
reverse :: List a -> List a
reverse = undefined

-- (*) ��� ��������� ������� ������
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = undefined

-- (*) ��� ������������ ��������� ������� ������
permutations :: List a -> List (List a)
permutations = undefined

-- (*) ���� ������. ��� ������������ ��������� ������� ������
-- ������ ��������
permutations' :: List a -> List (List a)
permutations' = undefined

-- ��������� ������� ����������� ����� ���
repeat :: a -> List a
repeat = undefined

-- ����� ������
-- ��������� ����� ������ ����������:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z l = undefined

-- ��� �� foldl, �� � ������ ����������� ��� ������������� ����������
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl = undefined

-- ������ ������
-- ��������� ����� ������ ����������:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z l = undefined

-- ����������
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr = undefined

-- ������ ����������� �� �������� �����
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- ��������� f � ������� �������� ������
map :: (a -> b) -> List a -> List b
map f l = undefined

-- ��������� ������ ������� � ������
concat :: List (List a) -> List a
concat = undefined

-- ���������� (concat . map), �� �����������
concatMap :: (a -> List b) -> List a -> List b
concatMap = undefined

-- �������� ��� ������ � ������ ��� ������ min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip a b = undefined

-- ����������, �� ������� ��� ������ �������, � �� ������������� Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith = undefined