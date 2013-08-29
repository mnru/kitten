{-# LANGUAGE BangPatterns #-}

module Kitten.Util.List
  ( (!?)
  , count
  , list
  , mapHead
  , stripCommonPrefix
  ) where

(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? pred n
[] !? _ = Nothing

count :: (Eq a) => a -> [a] -> Int
count x = go 0
  where
  go !acc (y:ys)
    | x == y = go (acc + 1) ys
    | otherwise = go acc ys
  go !acc [] = acc

list :: a -> [a]
list = (:[])

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x : xs) = f x : xs
mapHead _ [] = []

stripCommonPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
stripCommonPrefix (x : xs) (y : ys) | x == y = stripCommonPrefix xs ys
stripCommonPrefix xs ys = (xs, ys)
