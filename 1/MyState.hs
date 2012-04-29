module MyState where

instance Monad (State s) where  
  return x = State $ \s -> (x,s)  
  (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                      (State g) = f a  
                                  in  g newState


newtype State s a = State { runState :: s -> (a,s) }