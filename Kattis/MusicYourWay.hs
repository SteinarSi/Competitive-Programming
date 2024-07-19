import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (scanl', sortOn)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (ks:n:rest) <- C.getContents <&> C.lines

    let attributes = M.fromList (zip (C.words ks) [0..])

    splitAt (readInt n) rest
        & map (id &&& C.words)
                  ***
              (tail >>> map (attributes M.!))
        & uncurry (scanl' (\songs sort -> sortOn (snd >>> (!!sort)) songs))
        & tail
        & map (map fst >>> (ks:) >>> C.unlines)
        & C.unlines
        & C.init
        & C.putStr

format :: C.ByteString -> [(C.ByteString, [C.ByteString])] -> C.ByteString
format keys = map fst
        >>> (keys:)
        >>> C.unlines

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
