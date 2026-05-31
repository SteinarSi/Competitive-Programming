import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [_,xs,ys] <- C.getContents <&> C.lines
    C.zipWith diff xs ys
        & sum
        & print

diff :: Char -> Char -> Int
diff x y = min ((x'-y') `mod` 26) ((y'-x') `mod` 26)
  where
    x' = ord x - ord 'A'
    y' = ord y - ord 'A'
