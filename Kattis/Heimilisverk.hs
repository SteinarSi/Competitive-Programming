import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> heimilisverk S.empty
        >>> C.unlines
        >>> C.putStr
    )

heimilisverk :: S.Set C.ByteString -> [C.ByteString] -> [C.ByteString]
heimilisverk done [] = []
heimilisverk done (x:xs) | S.member x done =     heimilisverk done              xs
                         | otherwise       = x : heimilisverk (S.insert x done) xs
