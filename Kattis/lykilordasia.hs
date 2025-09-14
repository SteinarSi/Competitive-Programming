import           Control.Arrow              ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char                  (isAlpha, isDigit)

main :: IO ()
main = C.interact (C.words >>> map censor >>> C.unwords)

censor :: C.ByteString -> C.ByteString
censor xs
    | C.any isDigit xs && C.any isAlpha xs = C.replicate (C.length xs) '*'
    | otherwise = xs
