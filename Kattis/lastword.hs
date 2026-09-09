import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs:_:ys <- C.getContents <&> C.lines

    let (s,l) = ys
            & map (C.words >>> map readInt)
            & foldl' (\(s,_) [s',l'] -> (s+s',l')) (0,C.length xs)

    C.drop s xs
        & C.take l
        & C.putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
