import           Data.Bits (shiftL, (.&.))
import           Data.Bool (bool)
import           Data.List (intersperse, transpose)

main :: IO ()
main = do
    [a,b,c,d] <- fmap (map (read . pure)) getLine
    putStr . unlines . transpose $ intersperse "    " [
                toLED a,
                toLED b,
                "    ",
                toLED c,
                toLED d
            ]

toLED :: Int -> String
toLED d = map (\s -> bool '.' '*' (shiftL (1::Int) s .&. d > 0)) [3,2,1,0]
