import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (
            C.words
        >>> tail
        >>> map readInt
        )

    let c = map (sum xs -) xs
            & S.fromList
            & S.toAscList

    print (length c)

    map (show >>> C.pack) c
        & C.unwords
        & C.putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
