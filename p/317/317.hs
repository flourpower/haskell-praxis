import qualified Data.Set as S	

strDiff :: String -> String -> String
--Remove from the first string any characters in the second string
strDiff str str' = filter (\c -> c `S.notMember` set) str 
  where set = S.fromList str'