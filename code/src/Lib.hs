{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Prelude hiding (not, even, odd)

not :: Bool -> Bool
not True = False
not False = True

even :: Integer -> Bool
even m = m `mod` 2 == 0

divisibleBy :: Integer -> Integer -> Bool
divisibleBy k n = mod n k == 0

even' = divisibleBy 2
odd = not . even

type Pred = Integer -> Bool

and :: Pred -> Pred -> Pred
and p1 p2 = \a -> (p1 a) && (p2 a)
or  p1 p2 = \a -> (p1 a) || (p2 a)

lift :: (Bool -> Bool -> Bool)
     -> Pred
     -> Pred
     -> Pred
lift f p1 p2 = \a -> f (p1 a) (p2 a)
and' = lift (&&)
or'  = lift (||)

sumr :: [Integer] -> Integer
sumr [] = 0
sumr (x:xs) = x + (sumr xs)

productr [] = 1
productr (x:xs) = x * (productr xs)

foldright :: b
          -> (a -> b -> b)
          -> [a]
          -> b
foldright z _ [] = z
foldright z f (x:xs) =
  f x (foldright z f xs)

sumr' = foldright 0 (+)
productr' = foldright 1 (*)

-- sumr [1,2,3,4]
-- 1 + sumr [2, 3, 4]
-- 1 + 2 + sumr [3, 4]
-- 1 + 2 + 3 + sumr [4]
-- 1 + 2 + 3 + 4 + sumr []
-- 1 + 2 + 3 + 4 + 0
-- 1 + 2 + 3 + 4
-- 1 + 2 + 7
-- 1 + 9
-- 10

suml :: [Integer] -> Integer
suml xs = go 0 xs
  where go accum [] = accum
        go accum (x:xs) = go (x + accum) xs

-- suml [1, 2, 3, 4]
-- go 0 [1, 2, 3, 4]
-- go 1 [2, 3, 4]
-- go 3 [3, 4]
-- go 6 [4]
-- go 10 []
-- 10

foldleft :: b
         -> (b -> a -> b)
         -> [a]
         -> b
foldleft z f xs = go z xs
  where go accum [] = accum
        go accum (y:ys) = go (f accum y) ys

suml' = foldleft 0 (+)

reverse = foldleft [] (flip (:))

map' :: (a -> b) -> [a] -> [b]
map' f = foldright [] mapf
  where mapf a accum = (f a) : accum


headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x:_) = Just x

fold :: b -> (a -> b) -> Maybe a -> b
fold z _ Nothing = z
fold _ f (Just x) = f x

headeither :: [a] -> Either String a
headeither [] = Left "Empty list"
headeither (x:_) = Right x
