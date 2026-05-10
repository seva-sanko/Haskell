module Lib
  ( isCongruent
  , Tree(..)
  , treeDepth
  ) where

modulus :: Int -> Int -> Int
modulus a b
  | b == 0    = 0
  | otherwise = a `mod` abs b

isCongruent :: Int -> Int -> Int -> Bool
isCongruent a b m -- = False
  | m == 0    = error "Модуль не может быть равен 0" --убрать строку проверки
  | otherwise = (a `mod` m) == (b `mod` m)
  -- isCongruent a b m = (a + b) `mod` m == 0

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)
--treeDepth (Node _ left right) = max (treeDepth left) (treeDepth right)