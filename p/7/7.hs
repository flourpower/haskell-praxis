--Solve the logic puzzle.
import Data.List
data Person = Baker | Cooper | Fletcher | Miller | Smith deriving (Eq,Ord,Read,Show)

basic = [Baker,Cooper,Fletcher,Miller,Smith]

--We starts a convention that these arrangements are top to bottom

arrangements = permutations basic

--we want to select from arrangements those lists that satisfy all of our predicates
p xs = and $ map ($ xs) [p1,p2,p3,p4,p5,p6]

answers = filter p arrangements

--Baker does not live on top.
p1 xs = (head xs) /= Baker 
	
--Cooper does not live on bottom
p2 xs = (last xs) /= Cooper

--Fletcher does not live on either the top or the bottom
p3 xs = ((head xs) /= Fletcher) && ((last xs) /= Fletcher)

--Miller is higher than cooper
p4 xs = m < c
  where m = unsafe_index Miller xs
        c = unsafe_index Cooper xs

--Smith is not adjacent to fletcher
p5 xs = not (adjacent xs Smith Fletcher)

--Fletcher is not adjacent to cooper
p6 xs = not (adjacent xs Fletcher Cooper)


adjacent :: [Person] -> Person -> Person -> Bool
adjacent people p1 p2
  | (abs $ k1 - k2) <= 1 = True
  | otherwise            = False
    where k1 = unsafe_index p1 people
          k2 = unsafe_index p2 people

unsafe_index el xs = helper 0 el xs
helper k el (x:xs)
  | el == x = k
  | otherwise = helper (k+1) el xs