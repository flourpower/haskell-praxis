--I get to use the state monad!
import Data.Char
import MyState
import Stack
import Util

--now we want a process function that processes these lists of strings
--we want an action that takes a list of strings then runs react on each of them
--let's try doing it one string at a time

react :: [Char] -> State [Double] Double
react string
  | isNum string = reactNum string
  | otherwise    = reactOp string


process strings = do
  sequence $ map react strings

--reactNum :: (Fractional a) => String -> State (Stack a) a
reactNum :: String -> State [Double] Double
reactNum string = State $ \stack -> (k,k:stack)
  where k = numify string

reactOp :: (Fractional a) => String -> State (Stack a) a
reactOp string = apply (toOp string)

toOp :: (Fractional a) => [Char] -> a -> a -> a
toOp "+" = (+)
toOp "-" = (subtract)
toOp "*" = (*)
toOp "/" = (/)

apply :: (a -> a -> b) -> State (Stack a) b
apply f = do
  a <- pop
  b <- pop
  return (f a b)

