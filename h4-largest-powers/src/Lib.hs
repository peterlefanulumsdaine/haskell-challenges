module Lib
    ( largestPowersInt
    ) where

import           Data.List
import           Data.Semigroup

class Eq a => Iterable a where
    zer :: a
    inc :: a -> a
    dec :: a -> a

instance Iterable Int where
    zer = 0
    inc = succ
    dec = pred

instance Iterable Integer where
    zer = 0
    inc = succ
    dec = pred

largestPowersInt :: Int -> [Int]
largestPowersInt = largestPowers

largestPowers0 :: Iterable a => Int -> [a]
largestPowers0 n = map inc lpn
  where
    lpn = replicate (n-1) zer ++ ntersperse (map inc lpn)
    ntersperse (a:as) = (a : replicate (n-1) zer) ++ (ntersperse as)

largestPowers1 :: Iterable a => Int -> [a]
largestPowers1 n = map inc lpn
  where
    lpn = replicate (n-1) zer ++ foldr (\a l -> a : replicate (n-1) zer ++ l) [] (map inc lpn)

largestPowers2 :: Iterable a => Int -> [a]
largestPowers2 n = lpn
  where
    lpn = replicate (n-1) (inc zer) ++ foldr (\a l -> a : replicate (n-1) (inc zer) ++ l) [] (map inc lpn)

largestPowers3 :: Iterable a => Int -> [a]
largestPowers3 n = lpn
  where
    lpn = seg ++ foldr (\a l -> a : seg ++ l) [] (map inc lpn)
    seg = replicate (n-1) (inc zer)
    
largestPowers = largestPowers3