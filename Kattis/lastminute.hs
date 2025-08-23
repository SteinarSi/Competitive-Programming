import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [au, bu, ar, br] <- C.getContents <&> (C.words >>> map readNum)

    print $ solve au bu ar br

solve :: Integer -> Integer -> Integer -> Integer -> Integer
solve au bu 0  0  = min au bu
solve au bu ar 0  = bu
solve au bu 0  br = au
solve au bu ar br = ar * br + au + bu

readNum :: Num a => C.ByteString -> a
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
