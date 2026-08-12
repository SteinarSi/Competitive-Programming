import           Control.Arrow         ((>>>))
import           Data.Array.ST         (newArray, readArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (assocs)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Tuple            (swap)

main :: IO ()
main = do
    [xs,ys] <- C.getContents <&> C.lines
    zipWith (\(x,c1) (_,c2) -> (x, max c1 c2)) (count xs) (count ys)
        & filter (snd >>> (>0))
        & concatMap (swap >>> uncurry replicate)
        & putStrLn

count :: C.ByteString -> [(Char,Int)]
count xs = assocs $ runSTUArray $ do
    ret <- newArray ('a','z') 0
    C.foldl' (\r x -> r >> readArray ret x >>= (succ >>> writeArray ret x)) (pure ()) xs
    pure ret
