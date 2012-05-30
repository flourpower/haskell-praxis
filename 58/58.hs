import Data.List

frames [x] = [(x,0)]
frames [x,y] = [(x,y)]
frames (x:y:xys) = (x,y):(frames xys)

bowl pins = foldl' process (0,0) $ frames pins

process (bonus,score) (x,y) = (bonus',score')
  where bonus' = newBonus (x,y) (bonus,score)
        score' = newScore (x,y) (bonus,score)

newBonus (x,y) (bonus,score)
  | x == 10   = 2
  | x+y == 10 = 1
  | otherwise = 0

newScore (x,y) (bonus,score)
  | bonus == 2 = x + x + y + y + score
  | bonus == 1 = x + x + y + score
  | otherwise  = x + y + score

testGame = [1, 4, 4, 5, 6, 4, 5, 5, 1, 0, 0 ,1 ,7 ,3 ,6 ,4 ,10, 0, 2, 8, 6]
--this isn't working right. will get to it tomorrow.