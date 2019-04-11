{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Prelude hiding (not, even, odd, sum, product)

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

-- 
-- Higher-kinded functions (lifting)
-- 
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

--
-- Lists
-- 
sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Integer] -> Integer
product [] = 1
product (x:xs) = x * product xs

foldright :: (a -> b -> b)
          -> b
          -> [a]
          -> b
foldright _ z []     = z
foldright f z (x:xs) = f x (foldright f z xs)

sumr' :: [Integer] -> Integer
sumr' = foldright (+) 0

-- foldright (+) 0 [1, 2, 3, 4]
-- 1 + (foldright (+) [2, 3, 4])
-- 1 + 2 + (foldright (+) [3, 4])
-- 1 + 2 + 3 + (foldright (+) [4])
-- 1 + 2 + 3 + 4 + (foldright (+) [])
-- 1 + 2 + 3 + 4 + 0
-- 1 + 2 + 3 + 4
-- 1 + 2 + 3
-- 1 + 2
-- 1

-- (1 + (2 + (3 + (4 + 0))))

product' :: [Integer] -> Integer
product' = foldright (*) 1

suml :: [Integer] -> Integer
suml xs = go xs 0
  where
    go :: [Integer] -> Integer -> Integer
    go [] accum = accum
    go (x:xs) accum = go xs (x + accum)

foldleft :: (b -> a -> b)
         -> b
         -> [a]
         -> b
foldleft f z xs = go xs z
  where go [] accum = accum
        go (y:ys) accum = go ys (f accum y)


-- foldleft (+) 0 [1, 2, 3, 4]
-- go [1, 2, 3, 4] 0
-- go [2, 3, 4] 1
-- go [3, 4] 3
-- go [4] 6
-- go [] 10
-- 10

-- ((((0 + 1) + 2) + 3) + 4)


reverse :: [a] -> [a]
reverse = foldleft (flip (:)) []
-- :t (:)       :: a -> [a] -> [a]
-- :t flip (:)  :: [a] -> a -> [a]
