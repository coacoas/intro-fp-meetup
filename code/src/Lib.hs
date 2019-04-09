{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Prelude hiding( even
                     , odd
                     , not
                     )

import Data.Functor
import Control.Applicative
import Control.Monad

not :: Bool -> Bool
not True  = False
not False = True

even :: Integer -> Bool
even n = n `mod` 2 == 0

-- Return true if the second integer is divisible by the first integer
divisibleBy :: Integer -> Integer -> Bool
divisibleBy k n = mod n k == 0
divisibleBy' k = \n -> mod n k == 0
divisibleBy'' k = (== 0) . flip (mod)

even' :: Integer -> Bool
even' = divisibleBy 2

type Pred a = a -> Bool

notP :: Pred a -> Pred a
notP p = \a -> not (p a)

odd :: Pred Integer
odd = notP even

odd' :: Integer -> Bool
odd' = not . even -- (f . g) <=> \x -> f(g(x))

and :: Pred a -> Pred a -> Pred a
and p1 p2 = combined
  where combined a = p1 a && p2 a

or :: Pred a -> Pred a -> Pred a
or p1 p2 = \a -> (p1 a) || (p2 a)

lift :: (Bool -> Bool -> Bool) -- f
     -> Pred a -- b1
     -> Pred a -- b2
     -> Pred a -- result
lift f b1 b2 = \a -> f (b1 a) (b2 a)

lift' :: (b -> b -> b)
      -> (a -> b)
      -> (a -> b)
      -> (a -> b)
lift' f p1 p2 = \a -> f (p1 a) (p2 a)

and' :: Pred a -> Pred a -> Pred a
and' = lift (&&)

or' :: Pred a -> Pred a -> Pred a
or' = lift (||)

power :: Integer -> Integer -> Integer
power n k =
  if (k <= 0)
  then 1
  else n * power n (k - 1)

power' :: Integer -> Integer -> Integer
power' n k = go 1 k
  where go :: Integer -> Integer -> Integer
        go accum pow =
          if (pow <= 0)
          then accum
          else go (n * accum) (pow - 1)


headsafe :: [a] -> Maybe a
headsafe [] = Nothing
headsafe (x:xs) = Just x

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe z _ Nothing  = z
foldMaybe _ f (Just a) = f a

headsafe' :: [a] -> Either String a
headsafe' [] = Left "Can't take head of an empty list"
headsafe' (x:xs) = Right x

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither l _ (Left a)  = (l a)
foldEither _ f (Right b) = (f b)

