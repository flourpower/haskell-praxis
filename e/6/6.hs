sum_of_squares = sum . map (^2) $ [1,2..100]

square_of_sums = (^2) . sum $ [1,2..100]

answer = abs (sum_of_squares - square_of_sums)