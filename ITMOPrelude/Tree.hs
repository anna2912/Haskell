{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error,)
import ITMOPrelude.Primitive

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show	
	
emptyTree = Leaf	
	
addToRoot x a = Node x a Leaf

addToLeft x Leaf = Node x Leaf Leaf
addToLeft x (Node a left right) = Node a (addToLeft x left) right

addToRight x (Node a left right) = Node a left (addToRight x right)
addToRight x Leaf = Node x Leaf Leaf

rotateLeft (Node x left (Node y left' right')) = Node y (Node x left left') right'

rotateRight (Node x (Node y left' right') right) = Node y left' (Node x right' right)

tmap :: (a -> b) -> Tree a -> Tree b
tmap f  Leaf = Leaf
tmap f (Node a left right) = Node (f a) (tmap f left) (tmap f right)

foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f a Leaf = a
foldr f a (Node x left right) = f x (foldr f (foldr f a right) left)