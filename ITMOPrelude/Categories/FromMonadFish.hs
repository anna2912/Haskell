{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish

-- ���
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

-- ������ �� ���
instance MonadFish m => Monad where
    return = returnFish
    f >>= g = (id >=> g) f

instance MonadFish m => Functor m where
    fmap f x = (id >=> (returnFish . f)) x

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join ma = (id >=> id) ma