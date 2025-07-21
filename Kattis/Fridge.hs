import           Control.Arrow         ((>>>))
import           Data.Array.Base       (UArray, assocs, (!))
import           Data.Array.ST         (newArray, readArray, runSTUArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)
import           Data.Function         (on, (&))
import           Data.List             (minimumBy)
import           Data.Tuple            (swap)

main :: IO ()
main = C.getLine >>= (
        count
    >>> smol
    >>> putStrLn)

smol :: UArray Char Int -> String
smol cnt | length strat1 < length strat2 = strat1
         | otherwise                     = strat2
  where
    strat1 = assocs cnt
        & drop 1
        & minimumBy (compare `on` swap)
        & (\(x,c) -> replicate (c+1) x)
    strat2 = '1' : replicate (cnt ! '0' + 1) '0'

count :: C.ByteString -> UArray Char Int
count xs = runSTUArray $ do
    ret <- newArray ('0','9') 0
    C.foldr (\x -> ((readArray ret x >>= (succ >>> writeArray ret x)) >>)) (pure ()) xs
    pure ret
