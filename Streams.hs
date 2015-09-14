
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Streams where
import Control.Applicative
import Data.Monoid


data Stream a = Cons a (Stream a) 

instance Show a => Show (Stream a) where
        show str = show $ take 100 $ streamToList str

instance Fractional (Stream Integer ) where
        (/) a@(Cons a0 a') b@(Cons b0 b') = Cons (a0 `div` b0) $ streamMap ( `div` b0) (a' - b'* (a / b))

instance Num (Stream Integer) where
        fromInteger n = Cons n (streamRepeat 0)
        negate s = streamMap (negate) s
        (+) (Cons a1 s1) (Cons a2 s2) = Cons (a1 + a2) ((+) s1 s2)
        (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) ((mulConst a0 b') + (a' * b))

instance Functor (Stream) where
        fmap f (Cons x sx) = (Cons (f x)) (fmap f sx)

instance Monoid (Stream Integer) where
        mempty = Cons (0) (streamRepeat (0))
        mappend x y  = combine (+) x y
        
instance Applicative Stream where
        pure x = Cons x (streamRepeat x)
        (Cons f sf) <*> (Cons x sx) = (Cons (f x)) (sf <*> sx)

mulConst :: Integer -> Stream Integer -> Stream Integer
mulConst n  = streamMap (*n) 

divConst :: (Fractional b) => b -> Stream b -> Stream b
divConst n = streamMap (/n)

streamToList :: Stream a -> [a]
streamToList (Cons x stream) = [x] ++ streamToList stream

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f ( Cons x stream ) = Cons (f x) (streamMap f stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons fa (streamFromSeed f fa) where
        fa = f a

combine ::(a -> a -> b) ->  Stream a -> Stream a -> Stream b
combine f (Cons a s1) (Cons b s2) = Cons ( f a b) $ combine f s1 s2


interleave :: Stream a -> Stream a -> Stream a
interleave (Cons a s1) (Cons b s2) = Cons a (Cons b (interleave s1 s2))


dropAlt :: Stream a -> Stream a
dropAlt (Cons _ (Cons b s)) = Cons b (dropAlt s)


nats :: Stream Integer
nats =  streamFromSeed (+1) 0



