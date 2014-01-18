import System
import Data.List

main = do
  [n,fileName] <- getArgs
  contents <- readFile fileName
  mapM print $ process (read n :: Int) contents

process n = (take n) . concat . map (nub) . (sortBy p) . group . sort . words
  where p x y = compare (genericLength y) (genericLength x)