import           Control.Arrow         ((>>>))
import           Control.Monad         (unless)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    key <- C.getLine <&> (C.words >>> tail >>> map readInt)
    unless (null key) $ do
        message <- C.getLine
        let k = length key
        let msg = message <> C.pack (replicate ((k - (C.length message `mod` k)) `mod` k) ' ')

        chunksOf k msg
            & map (encrypt key)
            & C.concat
            & quote
            & C.putStrLn

        main

encrypt :: [Int] -> C.ByteString -> C.ByteString
encrypt key msg = C.pack (map (pred >>> C.index msg) key)

quote :: C.ByteString -> C.ByteString
quote msg = C.pack "'" <> msg <> C.pack "'"

chunksOf :: Int -> C.ByteString -> [C.ByteString]
chunksOf k xs | C.null xs = []
              | otherwise = let (a,b) = C.splitAt k xs
                            in  a : chunksOf k b

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
