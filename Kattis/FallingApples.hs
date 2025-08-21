import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (transpose)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> C.transpose
        >>> map (C.unpack >>> fall 0)
        >>> transpose
        >>> unlines
        >>> putStr
    )

fall :: Int -> String -> String
fall a []       = replicate a 'a'
fall a ('.':xs) = '.' : fall a xs
fall a ('a':xs) = fall (a+1) xs
fall a ('#':xs) = replicate a 'a' <> "#" <> fall 0 xs
