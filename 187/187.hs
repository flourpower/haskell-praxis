--Given two strings, determine if all the characters in the second string
--appear in the first string; thus, DA is a subset of ABCD. Counts matter,
--so DAD is not a subset of ABCD, since there are two D in the second 
--string but only one D in the first string. 

import qualified Data.Map as M

subset :: String -> String -> Bool
subset str str' = (mset str) `less` (mset str')

mset :: String -> M.Map Char Int
mset = foldl update M.empty

less :: M.Map Char Int -> M.Map Char Int -> Bool
less mp mp' = (all (== Just True)) . (map (check mp mp')) $ (M.keys mp)

check :: M.Map Char Int -> M.Map Char Int -> Char -> (Maybe Bool)
check mp mp' key = do
  v  <- M.lookup key mp
  v' <- M.lookup key mp'	
  return (v <= v')

update :: (M.Map Char Int) -> Char -> (M.Map Char Int)
update mp chr = case (M.lookup chr mp) of
	                 Nothing -> M.insert chr 1 mp
	                 (Just count) -> M.insert chr (count + 1) mp