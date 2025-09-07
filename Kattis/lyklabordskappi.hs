import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust, fromMaybe)

main :: IO ()
main = do
    _:l:t:xs <- C.getContents <&> C.words
    let n = readInt l * readInt t
    warrior n n (map C.unpack xs)
        & fromMaybe "/ff"
        & putStrLn

warrior :: Int -> Int -> [String] -> Maybe String
warrior _ _ [] = Just ""
warrior n c (x:xs)
    | m+1 > n = Nothing
    | n==c && m+1 <= n = (x<>) <$> warrior n (c-m-1) xs
    | m+1 <= c = ((' ':x)<>) <$> warrior n (c-m-1) xs
    | otherwise = ('\n':) <$> warrior n n (x:xs)
  where
    m = length x

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
