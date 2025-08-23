import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (find)
import qualified Data.Set              as S

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> map (C.words >>> drop 1)
        >>> install S.empty
        >>> C.unwords
    )

install :: S.Set C.ByteString -> [[C.ByteString]] -> [C.ByteString]
install _ [] = []
install seen (xs:xss) = x : install (S.insert x seen) xss
  where
    Just x = find (`S.notMember` seen) xs
