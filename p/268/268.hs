import Data.List
import qualified Data.Set as S

-- 1) Remove all duplicate characters from a string. Thus, “aaabbb” becomes “ab” and “abcbd” becomes “abcd”.

-- 2) Replace all runs of consecutive spaces with a single space. Thus, “a.b” is unchanged and “a..b” becomes “a.b”, using a dot to make the space visible.

--1
nubby :: (Ord a) => [a] -> [a]
--You could just do this with nub, but nub is O(n^2)
-- because it doesn't have the Ord constraint
nubby = reverse . fst . foldl combine ([],S.empty)

combine (xs,set) x = if (x `S.member` set)
	                  then (xs,set) 
	                  else (x:xs,S.insert x set)

--2
single_space = (concatMap f) . group

f (' ':xs) = " "
f str = str