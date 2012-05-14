import Data.List

bsearch n ns = help n (prepare ns)
  where prepare xs = zip xs [0,1..]

help n [] = Nothing
help n [(k, place)] = if n == k then Just place else Nothing
help n xs
  | n >= mid  = help n right
  | otherwise = help n left
    where midlen = floor $ (genericLength xs) / 2
          mid = fst $ xs !! midlen
          (left,right) = splitAt midlen xs
