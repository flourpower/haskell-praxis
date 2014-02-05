import Data.List

main = do
  print answer

next k
  | k `mod` 2 == 0 = k `div` 2
  | otherwise = 3*k+1

count k =   (+1) 
          . length
          . takeWhile (/= 1)
          . iterate next 
          $ k

answer =   fst 
         . head
         . sortBy (\(a,b) (c,d) -> compare d b)
         . map (\k -> (k,count k))
         $ [1,2..1000000]