{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons x xs) = Succ (length xs)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ x = x
(Cons x xs) ++ y = Cons x (xs ++ y)

-- Список без первого элемента
tail :: List a -> List a
tail Nil = error "empty list"
tail (Cons x xs) = xs

-- Список без последнего элемента
init :: List a -> List a
init Nil = error "empty list"
init (Cons x Nil) = Nil
init (Cons x xs) = Cons x (init xs)

-- Первый элемент
head :: List a -> a
head Nil = error "empty list"
head (Cons x xs) = x

-- Последний элемент
last :: List a -> a
last Nil = error "empty list"
last (Cons x Nil) = x
last (Cons x xs) = last xs

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero xs = Nil
take (Succ n) Nil = Nil
take (Succ n) (Cons x xs) = Cons x (take n xs)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero xs = xs
drop (Succ n) Nil = Nil
drop (Succ n) (Cons x xs) = drop n xs

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons x xs) = if' (p x) (Cons x (filter p xs)) (filter p xs)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons x xs) = case p x of
		Just y -> Cons y (gfilter p xs)
		Nothing -> gfilter p xs

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons x xs) = if' (p x) (Cons x (takeWhile p xs)) Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons x xs) = if' (p x) (dropWhile p xs) (Cons x xs)

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair Nil Nil
span p (Cons x xs) = if' (p x) (let (Pair ys zs) = span p xs in Pair (Cons x ys) zs) (Pair Nil (Cons x xs))

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p = span (not . p)

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons x xs) !! Zero = x
(Cons x xs) !! (Succ n) = xs !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse = reverse' Nil
  where reverse' y (Cons x xs) = reverse' (Cons x y) xs
        reverse' y Nil = y

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = subsequences xs ++ lmap (Cons x) (subsequences xs)

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
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
foldl f z Nil = z
foldl f z (Cons x xs) = foldl f (f z x) xs

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z (Cons x xs) = Cons (f z x) (scanl f (f z x) xs)

-- Правая свёртка
-- порождает такое дерево вычислений:
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
foldr f z Nil = z
foldr f z (Cons x xs) = f x (foldr f z xs)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Cons z Nil
scanr f z (Cons x xs) = Cons (f x r) rs where (Cons r rs) = scanr f z xs

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
lmap :: (a -> b) -> List a -> List b
lmap f Nil = Nil
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons x xs) = x ++ concat xs

-- Эквивалент (concat . lmap), но эффективнее
concatlmap :: (a -> List b) -> List a -> List b
concatlmap f Nil = Nil
concatlmap f (Cons x xs) = (f x) ++ concatlmap f xs

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip x Nil = Nil
zip Nil y = Nil
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) (zip xs ys)

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f xs Nil = Nil
zipWith f Nil ys = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)