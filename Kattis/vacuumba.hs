import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t <- fmap readInt C.getLine
    replicateM_ t $ do
        m <- fmap readInt C.getLine
        replicateM m C.getLine >>= (
                map (C.words
                    >>> map (C.unpack >>> read)
                    >>> (\[a,b] -> (2*pi*a/360,b))
                    )
                >>> solve (0,0) (pi/2)
                >>> C.putStrLn
            )

solve :: (Double,Double) -> Double -> [(Double,Double)] -> C.ByteString
solve (x,y) _ [] = C.unwords $ map (show >>> C.pack) [x,y]
solve (x,y) a ((theta,dist):xs) = solve (x+dx,y+dy) a' xs
    where a' = a+theta
          dx = cos a' * dist
          dy = sin a' * dist

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
