{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

import ITMOPrelude.Primitive

class Monoid m where
	mempty :: m
	mappend :: m -> m -> m

class Monoid g => Group g where
	gempty :: g
	gappend :: g -> g -> g
	ginv :: g -> g
	 
instance Monoid Nat where
	mempty = Zero
	mappend = (+.)
	
instance Monoid Int where
	mempty = intZero
	mappend = (.+.)
	
instance Group Int where
	gempty = intZero
	gappend = (.+.)
	ginv = intNeg
  
data MulInt = Mult Int

instance Monoid MulInt where
	mempty = Mult intOne
	mappend (Mult a) (Mult b) = Mult $ a .*. b
	
instance Monoid Rat where
	mempty = Rat intZero natOne
	mappend = (%+)
	
instance Group Rat where
	gempty = Rat intZero natOne
	gappend = (%+)
	ginv = ratNeg
