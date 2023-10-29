scm :: Integer -> Integer
scm n = foldr lcm 1 [1..n]