prime :: Integer -> Bool
prime n =   all (== False) 
          . map (\x -> n `mod` x == 0) 
          $ list n

answer = maximum . filter prime $ factors

factors = filter (\x -> 600851475143 `mod` x == 0) $ list 600851475143

list k = [2..(ceiling (sqrt (fromIntegral k)))]
