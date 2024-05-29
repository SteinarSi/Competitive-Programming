import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    xs <- C.getContents <&> (
            C.lines
        >>> map readInt
        >>> S.fromList
        >>> S.difference (S.fromList [0..n-1])
        >>> S.toAscList
        )

    forM_ xs print
    putStrLn ("Mario got " <> show (n - length xs) <> " of the dangerous obstacles.")

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
