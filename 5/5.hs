--Programming a cyclical list
--Josephus survives in the 31st position for (41,3)
import Data.List

j n m = helper [] m [1..n]

helper accum m [] = accum
helper accum m rest = helper (accum ++ [newGuy]) m newRest
  where newGuy = (cycle rest) !! (m-1)
        newRest = renew m rest

renew m xs = (takeWhile (/= guy)) . (drop m) . cycle $ xs
  where guy = (cycle xs) !! (m-1)

