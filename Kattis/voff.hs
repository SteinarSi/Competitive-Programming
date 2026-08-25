import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)
    concatMap (\x -> [(x,1),(x+k,-1)]) xs
        & sort
        & map snd
        & scanl (+) 0
        & maximum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
