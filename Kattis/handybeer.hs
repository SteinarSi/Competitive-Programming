import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [t,s] <- C.getLine <&> (C.words >>> map readInt)
    text <- C.getLine
    print (solve t s text)

solve :: Int -> Int -> C.ByteString -> Int
solve t s xs = dp (0,0) (C.length xs - 1)
    where
        dp :: (Int,Int) -> Int -> Int
        dp (l,r) (-1) = min l r
        dp (l,r) i | c == ' '              = dp (l+t,r+t) (i-1)
                   | S.member (ord c) left = dp (l+t, min (r+1000) (l+t+s)) (i-1)
                   | otherwise             = dp (min (l+1000) (r+t+s), r+t) (i-1)
            where c = C.index xs i

        left :: S.IntSet
        left = "qwertasdfgzxcvb "
                & map ord
                & S.fromList

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
