import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.readInt
        >>> fromJust
        >>> fst
        >>> (\m -> find (<=m) busNumbers)
        >>> maybe "none" show
        >>> putStrLn
    )

busNumbers :: [Int]
busNumbers = [373464,327763,320264,314496,262656,216125,216027,195841,171288,165464,149389,134379,110808,110656,65728,64232,46683,40033,39312,32832,20683,13832,4104,1729]
