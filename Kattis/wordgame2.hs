import           Control.Arrow         ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&), on)
import           Data.Functor          ((<&>))
import           Data.List             (delete, minimumBy)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.lines >>> drop 1)

    C.putStrLn $ case play xs of
        [] -> C.pack "-1"
        ys -> minimumBy (compare `on` (C.length &&& id)) ys

combine :: C.ByteString -> C.ByteString -> [C.ByteString]
combine xs ys = [1..min (C.length xs) (C.length ys)]
    & filter (\i -> C.takeEnd i xs == C.take i ys)
    & map (\k -> xs <> C.drop k ys)

play :: [C.ByteString] -> [C.ByteString]
play []  = error "bruh"
play [x] = [x]
play xs = do
    x <- xs
    y <- delete x xs
    xy <- combine x y
    play (xy : delete y (delete x xs))
