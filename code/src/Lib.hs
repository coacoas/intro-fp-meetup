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

