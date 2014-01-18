import Data.List

--remove every nth element from a list, in a weird way
nth n xs = (map fst) . filter (\(x,y) -> y `mod` n /= 0) $ xs'
  where xs' = zip xs [1,2..]

--remove from a list all duplicates except the first
unDup :: (Eq a) => [a] -> [a]
unDup = nub 

--split an input list in half
halve xs = (take k xs, drop k xs)
  where k = floor $ lngth / 2
        lngth = fromIntegral $ length xs