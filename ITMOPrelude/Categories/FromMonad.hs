{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- ���
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- ������ ���
instance Monad m => MonadFish m where
	returnFish = return
	f >=> g = \x -> (f x >>= g)
  
instance Monad m => Functor m where
	fmap f x = x >>= (return . f)

instance Monad m => MonadJoin m where
	returnJoin = return
	join ma = ma >>= id