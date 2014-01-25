digits n = digits_ n []

digits_ n xs
  | n < 0 = xs
  | n < 10 = n:xs
  | otherwise = (n `mod` 10):(digits_ ((n - (n `mod` 10)) `div` 10) xs)

palindrome n = digits n == (reverse $ digits n) 

answer = maximum [x*y | x <- [100..999], y <- [100..999], palindrome (x*y)]