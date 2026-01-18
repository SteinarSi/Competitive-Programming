import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (catMaybes)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.lines >>> drop 1 >>> zipWith clash [1..] >>> catMaybes)
    if null xs
        then putStrLn "INCOMPLETE"
        else print (snd (minimum xs))

clash :: Int -> C.ByteString -> Maybe (Int,Int)
clash p xs = royale (0,0) 0
  where
    royale :: (Int,Int) -> Int -> Maybe (Int,Int)
    royale (w,l) i
        | l >= 3              = royale (0,0) i
        | w >= 12             = Just (i,p)
        | i >= C.length xs    = Nothing
        | C.index xs i == 'W' = royale (w+1,l) (i+1)
        | otherwise           = royale (w,l+1) (i+1)
