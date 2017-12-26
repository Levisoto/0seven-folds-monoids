{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lib where

import Data.Monoid
import Sized

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data JoinListBasic a = Empty2
                     | Single2 a
                     | Append2 (JoinListBasic a) (JoinListBasic a)

jlbToList :: JoinListBasic a -> [a]
jlbToList Empty2        = []
jlbToList (Single2 a)   = [a]
jlbToList (Append2 l r) = jlbToList l ++ jlbToList r

------------------------------------------------------------------
-----------------Problem 1----------------------------------------
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty r = r
(+++) l r = Append (foldMap tag [l,r] ) l r

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

------------------------------------------------------------------
-----------------Problem 2----------------------------------------
indexJ ::(Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append m l r) 
  | i < sL = indexJ i l
  | i >= sL = indexJ (i-sL) r
  | i >= sm = Nothing
   where
     sL = (getSize.size.tag) l
     sR = (getSize.size.tag) r
     sm = (getSize.size) m
indexJ _ _ = Nothing

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 n@(Single _ a) = n
dropJ i (Append m l r) 
  | i < sL = (dropJ i l) +++ r
  | i == sL = r
  | i > sL = dropJ (i-sL) r
  | i >= sm = Empty
   where
     sL = (getSize.size.tag) l
     sR = (getSize.size.tag) r
     sm = (getSize.size) m
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i n@(Append m l r) 
  | i < sL = takeJ i l
  | i == sL = l
  | i > sL = l +++ (takeJ (i-sL) r)
  | i >= sm = n
   where
     sL = (getSize.size.tag) l
     sR = (getSize.size.tag) r
     sm = (getSize.size) m
takeJ _ _ = Empty
