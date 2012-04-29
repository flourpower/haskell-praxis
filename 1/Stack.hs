module Stack (Stack(..),push,pop) where
import MyState


type Stack a = [a]

pop :: State (Stack a) a  
pop = State $ \(x:xs) -> (x,xs)  

push :: a -> State (Stack a) ()  
push a = State $ \xs -> ((),a:xs)