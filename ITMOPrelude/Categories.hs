{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.List
import ITMOPrelude.Tree
  
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

