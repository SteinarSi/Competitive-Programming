import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, when)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n, m] <- C.getLine <&> (C.words >>> map readInt)
    when (n+m /= 0) $ do
        heads <- replicateM n C.getLine <&> (map readInt >>> sort)
        knights <- replicateM m C.getLine <&> (map readInt >>> sort)
        putStrLn $ maybe "Loowater is doomed!" show (slay heads knights)
        main

slay :: [Int] -> [Int] -> Maybe Int
slay [] _ = Just 0
slay _ [] = Nothing
slay (h:hs) (k:ks) | h > k     = slay (h:hs) ks
                   | otherwise = fmap (k+) (slay hs ks)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
