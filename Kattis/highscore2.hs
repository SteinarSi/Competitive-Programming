import Control.Monad (replicateM_)

main :: IO ()
main = getLine >>= flip replicateM_ (getLine >>= print . best . map read . words) . read

best :: [Integer] -> Integer
best [a,b,c,2] = maximum $ noSpread [a,b,c,2] : map score [[a+1,b+1,c],[a,b+1,c+1],[a+1,b,c+1]]
best [a,b,c,3] = maximum $ noSpread [a,b,c,3] : map score [[a+1,b+1,c+1],[a+2,b+1,c],[a+2,b,c+1],[a+1,b+2,c],[a,b+2,c+1],[a+1,b,c+2],[a,b+1,c+2]]
best [a,b,c,d] = noSpread [a,b,c,d]

noSpread :: [Integer] -> Integer
noSpread [a,b,c,d] = maximum $ map score [[a+d,b,c],[a,b+d,c],[a,b,c+d]]

score :: [Integer] -> Integer
score [a, b, c] = a^2 + b^2 + c^2 + 7 * minimum [a,b,c]
