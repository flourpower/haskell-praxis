--I could avoid that reversal by using difference lists,
--but the complexity is already worse than O(n) anyways.

--I first tried it using the modulo function as a filter.
--This misses the point - the sieve is fast because it doesnt use division!

--Now i fixed that, but it still seems very slow.

sieve n = helper [] ([2..n])

helper primes [] = reverse primes
helper primes (x:xs) = helper (x:primes) (dropEvery xs x)

answer = length $ sieve 15485863


dropEvery :: [a] -> Int -> [a]
dropEvery list count = helper list count count
  where helper [] _ _ = []
        helper (x:xs) count 1 = helper xs count count
        helper (x:xs) count n = x : (helper xs count (n - 1))

