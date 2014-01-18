--find the first non-repeating character in a string
import Data.List
import Data.Function

nrp :: (Ord a) => [a] -> Maybe a
nrp xs = safeHead .
         map fst . 
         sortBy (compare `on` snd) .
         concat .
         (filter (\xs -> length xs == 1)) .
         groupBy ((==) `on` fst) .
         sortBy (compare `on` fst) $ zip xs [1,2..]

safeHead [] = Nothing
safeHead xs = Just $ head xs
