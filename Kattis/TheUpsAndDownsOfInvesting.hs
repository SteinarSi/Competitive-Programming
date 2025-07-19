import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (isPrefixOf)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:n:m:xs <- C.getContents <&> (C.words >>> map readInt)
    let changes = zipWith compare xs (drop 1 xs)
        peaks   = matches (replicate (n-1) LT <> replicate (n-1) GT) changes
        valleys = matches (replicate (m-1) GT <> replicate (m-1) LT) changes
    putStrLn (show peaks <> " " <> show valleys)

matches :: Eq a => [a] -> [a] -> Int
matches mask [] = 0
matches mask xs | mask `isPrefixOf` xs = 1 + matches mask (drop 1 xs)
                | otherwise            =     matches mask (drop 1 xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
