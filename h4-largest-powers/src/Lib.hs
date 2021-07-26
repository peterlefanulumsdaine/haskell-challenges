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

-- We give two approaches:
-- one “list-based”, using the self-similarity of the sequence, very elegant and Haskelly, but a bit complicated to analyse/optimise for performance;
-- the other “counting-based”, simply counting up in base n and then counting the trailing zeros of each number; more naïve, and slower (at least at first), but with more clearly scalable performance (should be log-linear-time, constant/log space?)

-- Comparison in practice is interesting: In `ghci`, the list-based version is significantly quicker and (?)lower-memory than `largestPowers4`.  But under optimised compilation, running the test suite, the counting version fits into the restricted heap and passes the efficiency test better.


-- naïvely-written version of the fully list-based approach
largestPowers_list_0 :: Iterable a => Int -> [a]
largestPowers_list_0 n = map inc $ replicate (n-1) zer ++ expand (largestPowers_list_0 n)
  where
    expand (a:as) = (a : replicate (n-1) zer) ++ (expand as)

-- lightly optimised version of the fully list-based approach
largestPowers_list :: Iterable a => Int -> [a]
largestPowers_list n = lpn
  where
    seg = replicate (n-1) (inc zer)
    lpn = seg ++ foldr (\a l -> a : seg ++ l) [] (map inc lpn)

-- incrementation for base-n representations, for the counting-based approach 
increment_base :: Int -> [Int] -> [Int]
increment_base _ [] = [1]
increment_base n (a:as) | a <  n-1 = (a+1) : as
increment_base n (a:as) | a == n-1 = 0 : increment_base n as
-- assumes all list entries encountered are in [0..n-1]

-- counting-based approach
largestPowers_counting :: Int -> [Int]
largestPowers_counting n = map (+1) $ tail $ map leading_zeros $ iterate (increment_base n) []
  where
    leading_zeros l = length $ takeWhile (==0) l

largestPowers = largestPowers_counting