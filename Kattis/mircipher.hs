import           Control.Arrow ((>>>))
import           Data.Bits     (shiftL)
import           Data.Char     (chr, ord)

main :: IO ()
main = getContents >>= (
            words
        >>> last
        >>> zipWith (\s -> ord >>> subtract (ord 'A') >>> (+s) >>> (`mod` 26) >>> (+ord 'A') >>> chr) (map (shiftL 1) [0..])
        >>> putStrLn
    )
