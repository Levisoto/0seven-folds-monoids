{-# LANGUAGE FlexibleInstances #-}

module Lecture where

import Data.Monoid
import qualified Data.Map as M
import Data.Foldable hiding (toList)


data TravelGuide = TravelGuide {title :: String, authors :: [String], price :: Double }
  deriving (Show, Ord, Eq)

data BinaryTree = Node TravelGuide BinaryTree BinaryTree
                | Leaf
                deriving Show

treefind :: TravelGuide -> BinaryTree -> Maybe TravelGuide
treefind t (Node v l r) = case compare t v of
                            EQ -> Just v
                            LT -> treefind t l
                            GT -> treefind t r
treefind _ Leaf         = Nothing

treeInsert :: TravelGuide -> BinaryTree -> BinaryTree
treeInsert t n@(Node v l r) = case compare t v of
                            EQ -> n
                            LT -> treeInsert t l
                            GT -> treeInsert t r
treeInsert t Leaf         = Node t Leaf Leaf

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                | Leaf2
                deriving Show

treefind2 :: (Ord a) => a -> BinaryTree2 a -> Maybe a
treefind2 t (Node2 v l r) = case compare t v of
                            EQ -> Just v
                            LT -> treefind2 t l
                            GT -> treefind2 t r
treefind2 _ Leaf2         = Nothing

treeInsert2 :: (Ord a) => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) = case compare t v of
                            EQ -> n
                            LT -> treeInsert2 t l
                            GT -> treeInsert2 t r
treeInsert2 t Leaf2         = Node2 t Leaf2 Leaf2

newtype TravelGuidePrice = TravelGuidePrice TravelGuide 
  deriving Eq

instance Ord TravelGuidePrice where
  (TravelGuidePrice (TravelGuide t1 a1 p1)) <= (TravelGuidePrice (TravelGuide t2 a2 p2))=
    p1 < p2 || ( p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)) )


data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                      | Leaf3
                      deriving (Show, Eq, Ord)


treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r) = case compare v v2 of
                                      EQ -> Node3 v2 c2 l r
                                      LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
                                      GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3             = Node3 v c Leaf3 Leaf3


treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r) = case compare v v2 of
                                      EQ -> Node3 v2 c2 l r
                                      LT -> let newLeft = treeInsert4 v c l
                                                newCache = c2 <> cached newLeft <> cached r
                                              in Node3 v2 newCache newLeft r
                                      GT -> let newRight = treeInsert4 v c r
                                                newCache = c2 <> cached l <> cached newRight
                                              in Node3 v2 newCache l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3


cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty


newtype Min = Min Double deriving Show

instance Monoid Min where
  mempty = Min infinity where infinity = 1/0
  mappend (Min x) (Min y) = Min $ min x y


modifyTravelGuidePriceMap :: Double -> M.Map a TravelGuide -> M.Map a TravelGuide
modifyTravelGuidePriceMap m = M.map (\tg -> tg { price = m * price tg })

modifyTravelGuidePrice' :: Functor f => Double -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice' m = fmap (\tg -> tg { price = m * price tg })



data MyO = MyO Integer
  deriving (Show, Ord, Eq)

instance Monoid MyO where
  mempty = MyO 0
  mappend (MyO x) (MyO y) = MyO $ x+y

-- data TreeMyO = Node6 MyO TreeMyO
--              | Leaf

data Tree a = Empty | Leaf6 a | Node6 (Tree a) a
  deriving (Eq, Show, Ord)


instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Leaf6 x) = f x
   foldMap f (Node6 l k ) = foldMap f l <> f k

-- instance Foldable TreeMyO where
--   foldMap f Leaf6 = Leaf6
--   foldMap f (Node6 (MyO x) xs) = Node6 $ MyO (f x) <> foldMap f xs 
--
class Listable a where
  toList :: a -> [Int]
  
-- show
data Tree2 a = Empty2 | Node62 a (Tree2 a) (Tree2 a)

instance Listable (Tree2 Int) where
  toList Empty2        = []
  toList (Node62 x l r) = toList l ++ [x] ++ toList r
