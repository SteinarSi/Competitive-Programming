import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (findIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    name:_:rest <- C.getContents <&> C.lines
    rest
        & findIndex (\x -> (C.length x == C.length name) && diffs name x <=1)
        & fromJust
        & succ
        & print

diffs :: C.ByteString -> C.ByteString -> Int
diffs a b = C.zipWith (/=) a b
    & filter id
    & length
