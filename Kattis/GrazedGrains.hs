import           Control.Arrow         ((>>>))
import           Control.Monad         (guard)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

precision :: Double
precision = 0.1

main :: IO ()
main = do
    ufos <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.words >>> map (readInt >>> fromIntegral) >>> \[x,y,r] -> (x,y,r)))

    let covered (x,y) = any (\(p,q,r) -> (p-x)^2 + (q-y)^2 <= r*r) ufos
        points = length $ do
            x <- [-10,-10+precision .. 20]
            y <- [-10,-10+precision .. 20]
            guard (covered (x,y))

    print (fromIntegral points * precision^2)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
