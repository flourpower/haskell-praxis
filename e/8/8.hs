main = do
	contents <- getContents
	let num = process contents
	let answer = prods . fives $ num
	print answer

process = map (\c -> read [c] :: Int) . concat . lines

fives xs
  | length xs < 5 = []
  | otherwise = (take 5 xs) : (fives $ drop 1 xs)

prods = maximum . (map product) 