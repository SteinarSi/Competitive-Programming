import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (elemIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    putStrLn (solulu n xs)

solulu :: Int -> [Int] -> String
solulu n xs | total `mod` 3 /= 0 = "-1"
            | otherwise          = case first 0 0 xs of
                Just (i, j) -> show i <> " " <> show j
                _           -> "-1"
    where
        total = sum xs
        size = total `div` 3

        first :: Int -> Int -> [Int] -> Maybe (Int, Int)
        first _ _ [] = Nothing
        first tot i (x:xs) = case compare tot size of
            LT -> first (tot+x) (i+1) xs
            GT -> Nothing
            EQ -> second (tot+x) i (i+1) xs

        second :: Int -> Int -> Int -> [Int] -> Maybe (Int, Int)
        second _ _ _ [] = Nothing
        second tot i j (x:xs) = case compare tot (2*size) of
            LT -> second (tot+x) i (j+1) xs
            GT -> Nothing
            EQ -> Just (i, j)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
