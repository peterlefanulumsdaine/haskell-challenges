module Lib
    ( shortestLongest
    ) where

-- check if second list has ≤ length of first
-- terminates provided either list is finite
noLongerThan :: [a] -> [a] -> Bool
noLongerThan [] [] = True
noLongerThan [] (_:_) = False
noLongerThan (_:_) [] = True
noLongerThan (_:xs) (_:ys) = noLongerThan xs ys

-- “overprint” one list on another
-- here used to lazily find maximum length 
overprint :: [a] -> [a] -> [a]
overprint xs [] = xs
overprint [] ys = ys
overprint (x:xs) (_:ys) = x : overprint xs ys

-- overprint all of a given set of lists
-- to get (lazily!) a list whose length is the longest among them
longest :: [[a]] -> [a]
longest = foldr overprint []

-- “restrict” one list to the length of another
-- here used to lazily find minimum length
restrict :: [a] -> [a] -> [a]
restrict xs [] = []
restrict [] ys = []
restrict (x:xs) (_:ys) = x : restrict xs ys

-- restrict all of a given set of lists
-- to get a list whose length is the shortest among them
shortest :: [[a]] -> [a]
shortest = foldr restrict (repeat undefined)


shortestLongest :: [[[a]]] -> [[a]]
shortestLongest ls = [ as | l <- ls, as <- l,
                            noLongerThan theShortestLongest (longest l),
                            noLongerThan as theShortestLongest ]
  where theShortestLongest = shortest $ map longest ls
