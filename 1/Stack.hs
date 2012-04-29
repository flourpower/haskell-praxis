module Stack (Stack(..),push,pop,execute) where
import MyState


type Stack a = [a]

pop :: State (Stack a) a  
pop = State $ \(x:xs) -> (x,xs)  

push :: a -> State (Stack a) ()  
push a = State $ \xs -> ((),a:xs)

execute :: (Monad m) => [m b] -> m b
execute [action] = do action
execute (action:actions) = do
  action
  execute actions