-- a^2 + b^2 = c^2
-- a + b + c = 1000
-- c = (1000 - a - b)
--so a^2 + b^2 = (1000 - a - b)^2

tpl = [(a,b) |  a <- [1,2..1000]
              , b <- [1,2..1000] 
              , a + b <= 1000 
              , a < b
              , a^2 + b^2 == (1000 - a - b)^2
     ]

a = fst . head $ tpl
b = snd . head $ tpl
c = (1000 - a - b)

answer = a*b*c