--I get to use the state monad!
import Data.Char
import MyState
import Stack
import Util

--We call main with an empty stack.
main = do
  calculate []


calculate stack = do
  line <- getLine
  if (line == ":q") || (line == "")
	then print "Goodbye!"
	else do
		let (action,state) = runState (process (words line)) stack
		print (action,state)
		calculate state

--Combines reactNum and reactOp
react :: [Char] -> State [Double] Double
react string
  | isNum string = reactNum string
  | otherwise    = reactOp string

--Processes a whole list of things that are Doubles or Ops.
process :: [String] -> State [Double] Double
process strings = execute actions
  where actions = map react strings

--Reacts to an input of type double.  
reactNum :: String -> State [Double] Double
reactNum string = State $ \stack -> (k,k:stack)
  where k = numify string

--Reacts to an input representing a binary operation.
reactOp :: (Fractional a) => String -> State (Stack a) a
reactOp string = apply (toOp string)

--Convert a string representation of a binary op to that op.
toOp :: (Fractional a) => [Char] -> a -> a -> a
toOp "+" = (+)
toOp "-" = (subtract)
toOp "*" = (*)
toOp "/" = (/)

--Apply a binary operation to the stack.
apply :: (a -> a -> a) -> State (Stack a) a
apply f = do
  x <- pop
  y <- pop
  push (f x y)
  return (f x y)

