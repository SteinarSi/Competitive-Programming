main :: IO ()
main = print [ x | x <- [1..], evenlyDivisible x]

evenlyDivisible :: Int -> Bool
evenlyDivisible n = all ((==0) . mod n) [20, 19, 18, 17, 16, 14, 13, 11]

{-
2   | 2
3   | 3
4   | 4, 2
5   | 5
7   | 7
8   | 8, 4, 2

-}

{-
20 | 2 4 5 10
19 | 
18 | 2 3 6 9
17 | 
16 | 2 4 8 
15 | 3 5     !!
14 | 2 7
13 |
12 | 2 3 4 6 !!
11 | 
10 | 2 5 !!
9  | 3 !!
8  | 2 4 !!
7  | !!
6  | 2 3 !!
5  | !!
4  | 2 !!
3  | !!
2  | !!
1  | !!
-}




divi n = all ((==0) . mod n) [1..20]