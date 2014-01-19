--By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

answer = sum .
         filter (\x -> x `mod` 2 == 0) .
         takeWhile (<= 4000000) $
         fibs