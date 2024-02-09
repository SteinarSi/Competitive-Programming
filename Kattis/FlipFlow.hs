import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t:s:_:xs <- fmap (C.words >>> map readInt) C.getContents
    print (max 0 (flipFlow 0 s 0 xs - (t - last xs)))

flipFlow :: Int -> Int -> Int -> [Int] -> Int
flipFlow curr bot top []     = top
flipFlow curr bot top (x:xs) = flipFlow x (top - fallen) (bot + fallen) xs
    where fallen = min top delta
          delta = x - curr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
