{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lib2 where

import Data.Char
import Lib
import Sized
import StringBuffer
import Editor
import Buffer

newtype Score = Score Int
  deriving (Show, Eq, Ord)

instance Monoid Score where
  mempty                      = Score 0
  mappend (Score x) (Score y) = Score (x+y)

getScore :: Score -> Int
getScore (Score x) = x

score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg"         = Score 2
  | c' `elem` "bcmp"       = Score 3
  | c' `elem` "fhvwy"      = Score 4
  | c' `elem` "k"          = Score 5
  | c' `elem` "jx"         = Score 8
  | c' `elem` "qz"         = Score 10
  | otherwise              = Score 0
    where c' = toLower c

scoreString :: String -> Score
scoreString = foldMap score

scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

scrabble :: String -> JoinList (Score, Size) String
scrabble xs = Single (scoreString xs, Size 1) xs

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ xs) = xs
  toString (Append _ l r) = toString l ++ toString r
  fromString = foldr (+++) Empty . map scrabble . lines
    -- (Single (scoreString xs,Size 1) xs)
  line n b = indexJ n b
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b
  numLines = (getSize.size.tag)
  value = (getScore.fst.tag)


-- greeting :: String
-- greeting = unlines
--     [ "This buffer is for notes you don't want to save, and for"
--     , "evaluation of steam valve coefficients."
--     , "To load a different file, type the character L followed"
--     , "by the name of the file."
--     ]

-- -- Run the editor with the greeting as the initial buffer.

-- main = runEditor editor initialBuffer
--     where initialBuffer = (fromString greeting :: JoinList (Score, Size) String)
main = runEditor. editor .fromString.unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
