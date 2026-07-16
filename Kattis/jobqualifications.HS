import           Control.Arrow         (second, (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines
    let (qualifications,listings) = splitAt (readInt n) rest
            & S.fromList *** split
    listings
        & map (all (`S.member` qualifications) >>> bool "why bother?" "apply")
        & unlines
        & putStr

split :: [C.ByteString] -> [[C.ByteString]]
split [] = []
split (k:xs) = splitAt (readInt k) xs
    & second split
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
