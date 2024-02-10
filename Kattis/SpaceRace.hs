import           Control.Arrow ((>>>))
import           Data.Function (on)
import           Data.List     (maximumBy)

main :: IO ()
main = do
    d <- fmap read (getLine >> getLine)
    getContents >>= (
                lines
            >>> map (words >>> (\(a:b:c:_) -> (a,(read b,read c))))
            >>> maximumBy (compare `on` (\(_,(v,f)) -> v / (f / (d / v))))
            >>> fst
            >>> putStrLn
        )
