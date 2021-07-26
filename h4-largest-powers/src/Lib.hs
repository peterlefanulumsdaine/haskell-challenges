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

-- A different approach, which lower memory use, probably(?) at cost of speed

increment_base :: Int -> [Int] -> [Int]
increment_base _ [] = [1]
increment_base n (a:as) | a <  n-1 = (a+1) : as
increment_base n (a:as) | a == n-1 = 0 : increment_base n as
increment_base n _ = error "digits out of range"
-- speedup option: simplify checks, assume digits in [0..n-1]

largestPowers4 :: Int -> [Int]
largestPowers4 n = map (+1) $ tail $ map leading_zeros $ iterate (increment_base n) []
  where
    leading_zeros l = length $ takeWhile (==0) l

-- Interesting comparison: In `ghci`, the lazy list-based `largestPowers3` is significantly quicker and (?)lower-memory than `largestPowers4`.  But under optimised compilation, running the test suite, `largestPowers4` fits into the restricted heap and passes the efficiency test better.

largestPowers = largestPowers4