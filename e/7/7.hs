prime :: Integer -> Bool
prime 2 = True
prime n =   all (== False) 
          . map (\x -> n `mod` x == 0) 
          $ list n

primes = filter prime [2,3..]

list k = [2..(ceiling (sqrt (fromIntegral k)))]

answer = primes !! 10000