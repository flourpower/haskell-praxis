main = do
  print answer

prime :: Integer -> Bool
prime 2 = True
prime n =   all (== False) 
          . map (\x -> n `mod` x == 0) 
          $ list n

list k = [2..(ceiling (sqrt (fromIntegral k)))]

answer = sum . filter prime $ [2,3..2000000]