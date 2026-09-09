import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [t]:_:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    xs
        & sortOn last
        & solve (fromIntegral t)
        & print

solve :: Double -> [[Int]] -> Int
solve _ []         = 0
solve t ([s,d]:xs)
    | canDo >= s   = s + solve (t - circumference * fromIntegral s) xs
    | otherwise    = canDo
  where
    canDo = floor (t / circumference)
    circumference = pi * 2 * fromIntegral d

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
