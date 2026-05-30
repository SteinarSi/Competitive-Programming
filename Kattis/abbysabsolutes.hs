import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)

    xs
        & map (\x -> show (bool 1 n (n-x < x-1)))
        & unwords
        & putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
