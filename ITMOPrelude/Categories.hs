{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import Prelude (flip)

import ITMOPrelude.List
import ITMOPrelude.Tree
import ITMOPrelude.Primitive
  
class Functor f where
	fmap :: (a -> b) -> f a -> f b

class Monad m where
	return :: a -> m a	
	(>>=) :: m a -> (a -> m b) -> m b
	(>>) :: m a -> m b -> m b
	a >> b = a >>= \_ -> b

class Category cat where
	id  :: cat a a
	(.) :: cat b c -> cat a b -> cat a c

instance Functor List where
	fmap = lmap

instance Functor Tree where
	fmap = tmap
	
instance Functor Maybe where
	fmap f (Just a) = Just $ f a
	fmap f Nothing = Nothing

instance Monad List where
	return = flip Cons Nil
	xs >>= f = concatMap f xs
	
instance Monad Maybe where
	return = Just
	Nothing >>= _ = Nothing
	(Just x) >>= f = f x

-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return = State $ \s -> (s, a)
    state1 >>= f = State $ \s -> let (state2, a) = runState state1 s in runState (f a) state2