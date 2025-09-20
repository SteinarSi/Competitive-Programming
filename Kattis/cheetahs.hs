import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map (readInt >>> fromIntegral) >>> head &&& last))

    let
        score :: Double -> Double
        score when = map (\(t,v) -> (when-t)*v) xs
            & maximum &&& minimum
            & uncurry (-)

        bin :: Double -> Double -> Double
        bin lo hi
            | abs (s1-s2) <= 0.0001 = s1
            | s1 <= s2  = bin lo mi2
            | otherwise = bin mi1 hi
          where
            w = (hi-lo) / 3
            mi1 = lo + w
            mi2 = hi - w
            s1 = score mi1
            s2 = score mi2

    print (bin (maximum (map fst xs)) 100000000)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
