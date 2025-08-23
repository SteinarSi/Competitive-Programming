import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.filter isAlpha >>> abbreviate)
        >>> C.unlines
        >>> C.putStr
    )

abbreviate :: C.ByteString -> C.ByteString
abbreviate xs | C.length xs > 10 = C.pack (C.head xs : show (C.length xs - 2) ++ [C.last xs])
              | otherwise        = xs
