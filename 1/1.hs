--I get to use the state monad!
import Data.Char
import MyState
import Stack
import Util

reactNum :: String -> State [Double] ()
reactNum string = State $ \stack -> ((),(numify string):stack)

reactOp :: (Fractional a) => [Char] -> State (Stack a) a
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

