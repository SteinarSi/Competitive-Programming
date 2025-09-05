import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (findIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:x:xs <- C.getContents <&> (C.words >>> map readInt)
    findIndex (<=x) xs
        & maybe "infinity" (succ >>> show)
        & putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
