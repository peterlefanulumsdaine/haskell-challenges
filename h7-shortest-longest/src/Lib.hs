module Lib
    ( shortestLongest
    ) where

-- self-contained definition, all operations condensed + inlined 
shortestLongest :: [[[a]]] -> [[a]]
shortestLongest ls = filter (leq tsl) $ concat $ filter (flip leq tsl . longest) ls
  where
    leq = foldr (\y geqys xs -> not (null xs) && geqys (drop 1 xs)) (const True)
    longest = foldr (foldr (((.drop 1).).(.).(:)) id) []
    shortest = foldr (zipWith const) (repeat undefined)
    tsl = shortest $ map longest ls

-- human-readable version
shortestLongest' :: [[[a]]] -> [[a]]
shortestLongest' ls = [ aa | l <- ls, leqLength (longest l) theShortestLongest,
                            aa <- l, leqLength theShortestLongest aa ]
  where theShortestLongest = shortest $ map longest ls

-- check if first list has ≤ length of second
-- lazy; terminates if either list is finite
-- human-readable definition
leqLength' :: [a] -> [a] -> Bool
leqLength' [] [] = True
leqLength' [] (_:_) = True
leqLength' (_:_) [] = False
leqLength' (_:xs) (_:ys) = leqLength' xs ys

-- condensed definition
leqLength :: [a] -> [a] -> Bool
leqLength = foldr (\y geqys xs -> not (null xs) && geqys (drop 1 xs)) (const True)

-- “overprint” one list on another, human-readable definition
-- here used to lazily find maximum length
-- human-readable definition
overprint' :: [a] -> [a] -> [a]
overprint' [] ys = ys
overprint' (x:xs) [] = x:xs
overprint' (x:xs) (_:ys) = x : overprint xs ys

-- condensed defnition
overprint :: [a] -> [a] -> [a]
overprint = foldr (((.drop 1).).(.).(:)) id
-- slightly less condensed version:
--          foldr (\x opxs ys -> x : opxs (drop 1 ys)) id

-- overprint all of a given list of lists
-- to get (lazily!) a list whose length is the longest among them
-- note: not itself an element of the original list; it just represents the right length
longest :: [[a]] -> [a]
longest = foldr overprint []

-- “restrict” one list to the length of another
-- here used to lazily find minimum length
restrict :: [a] -> [a] -> [a]
restrict = zipWith const

-- restrict all of a given set of lists
-- to get a list whose length is the shortest among them
-- note: not itself an element of the original list; it just represents the right length
shortest :: [[a]] -> [a]
shortest = foldr restrict (repeat undefined)



test_basic =
            [ [ "a", "bc", "de", "f", "gh" ]
            , [ "ijk", "lm", "nop", "q"]
            , [ "rst" ]
            , [ "uv", "w", "x", "yz"]
            ]