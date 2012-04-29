--I get to use the state monad!
import Data.Char
import MyState
import Stack
import Util

--now we have a process function
--we can take a list of strings from input, run process, output result of computation, and pass new state back into the getline action

react :: [Char] -> State [Double] Double
react string
  | isNum string = reactNum string
  | otherwise    = reactOp string

process :: [String] -> State [Double] Double
process strings = execute actions
  where actions = map react strings
  
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

