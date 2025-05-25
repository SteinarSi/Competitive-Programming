import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [xs,ys] <- C.getContents <&> C.lines
    C.zipWith (==) xs ys
        & takeWhile id
        & length
        & (*2)
        & ((C.length xs + C.length ys) -)
        & print
