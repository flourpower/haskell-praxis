-- take a list of integers and a target number and determine if any 2 ints in the list sum to the target number
import qualified Data.Set as S	
	
check :: [Int] -> Int -> Bool
check xs int = any (== True) . (map (\x -> x `S.member` set)) $ xs
  where set = checkSet xs int 

checkSet :: [Int] -> Int -> (S.Set Int)
checkSet xs int = S.fromList . (map  (\x -> int - x)) $ xs 