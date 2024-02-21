import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    lengths <- C.getContents <&> (C.lines >>> map C.length)
    init lengths
        & map ((maximum lengths-) >>> (^2))
        & sum
        & print
