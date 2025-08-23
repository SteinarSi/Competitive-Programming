import           Control.Arrow            ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.Maybe               (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    xs <- C.getContents <&> (C.words >>> tail >>> map readInt)

    xs
        & map ((`S.findIndex` S.fromList xs) >>> show >>> C.pack)
        & C.unwords
        & C.putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
