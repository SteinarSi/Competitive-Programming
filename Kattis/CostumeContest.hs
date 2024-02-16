import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortBy, sortOn)
import           Data.Map              (assocs, empty, insertWith)
import           Data.Tuple            (swap)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> foldr (\c -> insertWith (+) c 1) empty
        >>> assocs
        >>> (\xs -> filter (snd >>> (minimum (map snd xs)==)) xs)
        >>> mapM_ (fst >>> C.putStrLn)
    )
