--compute the hamming weight of an arbitrary integer n
--that is, find the number of 1-bits in the binary representation of n

lg x = (log x) / (log 2)

lg' = floor . lg

weight n = snd $ foldl f (n,0) [(lg' n),((lg' n) - 1)..0]

f (num, count) pow = if 2^pow <= num then (num - 2^pow,count+1) else (num,count)
