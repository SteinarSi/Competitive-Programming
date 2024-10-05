import           Control.Arrow         ((>>>))
import           Data.Bits             (shiftL, shiftR)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    s:d:m:_ <- C.getContents <&> (C.words >>> map readBinary)
    let d' = fromIntegral (min 10000 d)
    C.putStrLn $ maybe (C.pack "Infinite money!") showBinary (solve s d' m)

solve :: Integer -> Int -> Integer -> Maybe Int
solve s d m | m `shiftR` (d-1) == 0 = Just (countdown m)
            | s `shiftR` (d-1)  > 0 = Nothing
            | otherwise             = fmap (d+) (solve s d (m `shiftR` d + s))

countdown :: Integer -> Int
countdown 0 = 0
countdown x = 1 + countdown (shiftR x 1)

readBinary :: C.ByteString -> Integer
readBinary = C.foldr' (\x (b,s) -> (2*b, bool s (s+b) (x=='1'))) (1,0) >>> snd

showBinary :: Int -> C.ByteString
showBinary = showBinary' >>> C.pack >>> C.reverse
    where showBinary' 0 = []
          showBinary' x | odd x     = '1' : showBinary' (x `div` 2)
                        | otherwise = '0' : showBinary' (x `div` 2)
