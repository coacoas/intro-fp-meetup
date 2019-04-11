{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Prelude hiding (not, even, odd)

data Boolean = TRUE | FALSE

not :: Bool -> Bool
not True = False
not False = True

even :: Integer -> Bool
even n = mod n 2 == 0
-- even n = n `mod` 2 == 0

divisibleBy :: Integer -> Integer -> Bool
divisibleBy k n = n `mod` k == 0
  
even' = divisibleBy 2
-- Function composition: (f ∘ g)(x) == f(g(x))
-- f :: b -> c ; g :: a -> b ; (f ∘ g) :: a -> c
odd   = not . even

-- Lifting
type Pred a = a -> Bool

and :: Pred a -> Pred a -> Pred a
and p1 p2 = \a -> (p1 a) && (p2 a)
or :: Pred a -> Pred a -> Pred a
or p1 p2 = \a -> (p1 a) || (p2 a)

-- Too much duplication... copy/paste is not your friend
lift :: (Bool -> Bool -> Bool)
     -> Pred a
     -> Pred a
     -> Pred a
lift f p1 p2 = \a -> f (p1 a) (p2 a)

and' :: Pred a -> Pred a -> Pred a
and' = lift (&&)
or'  :: Pred a -> Pred a -> Pred a
or'  = lift (||)
