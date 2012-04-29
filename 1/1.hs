--we want to make an rpn
import Data.Char
--import Control.Monad.State
--let's deal with using the calculator in the state monad and then coming back to everything else

type Stack a = [a]

pop :: State (Stack a) a  
pop = State $ \(x:xs) -> (x,xs)  
  
push :: a -> State (Stack a) ()  
push a = State $ \xs -> ((),a:xs)


r7 = do  
  a <- pop
  if a /= 7
    then push a
    else return ()



instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState


newtype State s a = State { runState :: s -> (a,s) }