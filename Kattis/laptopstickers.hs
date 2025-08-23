import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, guard)
import           Data.Array.Base       (newArray, writeArray, (!))
import           Data.Array.ST         (inRange, runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (range)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let b = ((0,0),(m-1,n-1))
        laptop = runSTUArray $ do
            arr <- newArray b '_'

            sequence_ $ do
                (c, [w,h,x,y]) <- zip ['a'..] xs
                p <- range ((y,x),(y+h-1,x+w-1))
                guard (inRange b p)
                pure (writeArray arr p c)

            pure arr

    forM_ [0..m-1] $ \y -> map (\x -> laptop ! (y,x)) [0..n-1] & putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
