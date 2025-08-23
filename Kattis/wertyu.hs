import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.Map.Strict       as M

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.unpack >>> map (keys M.!) >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

keys :: M.Map Char Char
keys = keyboard
        & concatMap (\xs -> zip (tail xs) xs)
        & M.fromList
    where
        keyboard = [
                "`1234567890-=",
                "QWERTYUIOP[]\\",
                "ASDFGHJKL;'",
                "ZXCVBNM,./",
                "  "
            ]
