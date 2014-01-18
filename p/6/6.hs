import Data.Char

lower = cycle ['a'..'z']
upper = cycle ['A'..'Z']

generic_rot c = head . drop 13 . dropWhile (/= c)

single_rot c
  | isLower c = generic_rot c lower
  | isUpper c = generic_rot c upper
  | otherwise = c

rot13 = map single_rot