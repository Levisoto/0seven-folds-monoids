module Lib where

import Data.Monoid

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
(+++) l r = Append (foldMap tag [l,r] ) l r

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m
