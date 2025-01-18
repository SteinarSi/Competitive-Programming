import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = do
    [n,m,k]:plots:circles:squares:_ <- C.getContents <&> (
                C.lines
            >>> map (C.words >>> map readInt)
        )

    let sizes = sortOn Down (map squareSize squares <> map fromIntegral circles)
        spots = sortOn Down (map fromIntegral plots)

    print $ fit spots sizes

fit :: [Double] -> [Double] -> Int
fit [] _ = 0
fit _ [] = 0
fit (r:rs) (x:xs) | x < r     = 1 + fit rs     xs
                  | otherwise =     fit (r:rs) xs

squareSize :: Int -> Double
squareSize s = sqrt 2 * (fromIntegral s / 2)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
