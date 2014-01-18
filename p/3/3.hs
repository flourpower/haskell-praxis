--first I'm supposed to compute the average number of calls before a single card recieves bingo

--let's make a bingo
import System.Random
import Data.List

main = do
  gen <- getStdGen
  print $ permute gen "fartmeister"

flop :: (RandomGen g) => g -> (Integer, g)
flop gen = randomR (0,1) gen

flops :: (RandomGen g) => g -> Integer -> [Integer]
flops gen n = genericTake n $ randomRs (0,1) gen

zipFlop :: (RandomGen g) => g -> [a] -> [(Integer,a)]
zipFlop gen xs = zip (flops gen len) xs
  where len  = genericLength xs

permute :: (RandomGen g) => g -> [a] -> [a]
permute gen xs = foldl' helper [] (zipFlop gen xs)
  where helper acc (0,x) = x:acc
        helper acc (1,x) = acc ++ [x]