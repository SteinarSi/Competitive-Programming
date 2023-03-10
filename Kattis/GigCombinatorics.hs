main :: IO()
main = getLine >> getLine >>= \l -> print (dynamics [read n::Int | n<-words l] 0 0 0 `mod` 1000000007)

dynamics :: Integral n => [n] -> n -> n -> n -> n
dynamics [] _ _ f3 = f3
dynamics (1:xs) f1 f2 f3 = let ff = f1+1                     in seq ff (dynamics xs ff f2 f3)
dynamics (2:xs) f1 f2 f3 = let ff = mod (f2*2+f1) 1000000007 in seq ff (dynamics xs f1 ff f3)
dynamics (3:xs) f1 f2 f3 = let ff = mod (f2+f3) 1000000007   in seq ff (dynamics xs f1 f2 ff)
dynamics _ _ _ _ = error "bruh"