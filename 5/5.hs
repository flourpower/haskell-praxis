--Programming a cyclical list
import Data.List

j n m = helper [] m [1..n]

helper accum m [] = accum
helper accum m rest = helper (accum ++ [newGuy]) m newRest
  where newGuy = (cycle rest) !! (m-1)
        newRest = renew m rest

renew m xs = right ++ left
  where left = init . (take m) $ xs
        right = drop m xs

safeTail [] = []
safeTail [x] = []
safeTail (x:xs) = xs