{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Categories.MonadJoin

-- ���
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

-- ������ �� ���
instance MonadJoin m => Monad m where
	return = returnJoin
	f >>= g = join (fmap g f)
  
instance MonadJoin m => MonadFish m where
	returnFish = returnJoin
	f >=> g = \a -> join (fmap g (f a))