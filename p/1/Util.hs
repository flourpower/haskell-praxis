module Util where

import Data.Char


numify :: String -> Double
numify k = read k :: Double

isNum :: [Char] -> Bool
isNum = all p

p :: Char -> Bool
p c = (isNumber c) || (c == '.')