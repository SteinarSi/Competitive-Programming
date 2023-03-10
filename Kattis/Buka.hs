main = do
    en <- getLine
    op <- getLine
    to <- getLine
    print (foo (read en) (read to) op)

foo :: Integer-> Integer->String->Integer
foo x y "*" = x*y
foo x y "+" = x+y