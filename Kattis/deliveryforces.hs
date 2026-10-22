import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    print (everyOther (drop (n `div` 3) (sort xs)))

everyOther :: [Int] -> Int
everyOther []       = 0
everyOther (x:_:xs) = x + everyOther xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
