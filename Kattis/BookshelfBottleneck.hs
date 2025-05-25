import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:h:xs <- C.getContents <&> (C.words >>> map readInt)
    putStrLn (solulu h 0 xs)

solulu :: Int -> Int -> [Int] -> String
solulu h ret [] = show ret
solulu h ret (x:y:z:xs) | null combs = "impossible"
                        | otherwise  = solulu h (ret + minimum combs) xs
  where
    combs = [((x,y),z),((x,z),y),((y,z),x)]
        & filter (snd >>> (<=h))
        & map (fst >>> uncurry min)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
