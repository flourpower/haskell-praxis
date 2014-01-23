--the solution to this is just 40 choose 20

answer = 40 `choose` 20

choose n k = (factorial n) / 
             (  (factorial k) * (factorial (n - k))  )

factorial 1 = 1
factorial n = n * (factorial (n-1))