import Data.Char

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

--we want to associate with each thing that could come in as a valiud input a stateful computation.

plus = do
  a <- pop
  b <- pop
  return (a+b)

minus = do
  a <- pop
  b <- pop
  return (a-b)

times = do
  a <- pop
  b <- pop
  return (a * b)

divide = do
  a <- pop
  b <- pop
  return ( a / b)


--I can't import Control.Monad.State because of some silly ambiguity issue?
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState


newtype State s a = State { runState :: s -> (a,s) }