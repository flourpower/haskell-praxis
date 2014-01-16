--You are given an array with integers between 1 and 1,000,000. One integer is in the array twice. How can you determine which one?
import qualified Data.Set as S

check list = snd $ foldl f (S.empty,Nothing) list
f (set,mint) int = if int `S.member` set then (set,Just int) else (S.insert int set,mint)