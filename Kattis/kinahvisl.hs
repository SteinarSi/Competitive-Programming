import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    x <- C.getLine
    y <- C.getLine
    print $ solve 1 (C.length x-1) x y

solve :: Int -> Int -> C.ByteString -> C.ByteString -> Int
solve ret (-1) _ _ = ret
solve ret i x y | C.index x i /= C.index y i = solve (ret+1) (i-1) x y
                | otherwise = solve ret (i-1) x y
