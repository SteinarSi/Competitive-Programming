import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (isPrefixOf, tails)

main :: IO ()
main = do
    [target,_,xs] <- C.getContents <&> C.lines
    C.tails xs
        & filter (C.isPrefixOf target)
        & length
        & print
