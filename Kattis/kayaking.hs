import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find, sortOn)
import           Data.Maybe            (fromJust, fromMaybe)

main :: IO ()
main = do
    [[b, n, e], [sb, sn, se], ks] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        choices :: [(Int,Int,Int)]
        choices = sortOn (\(u,v,w) -> u*sb + v*sn + w*se) [(2,0,0),(0,2,0),(0,0,2),(1,1,0),(1,0,1),(0,1,1)]

        speed :: Int -> Maybe Int
        speed v = speed' (b,n,e) ks
          where
            speed' :: (Int,Int,Int) -> [Int] -> Maybe Int
            speed' _ [] = Just maxBound
            speed' (rb,rn,re) (x:xs) = choices
                & find (\(cb,cn,ce) -> rb>=cb && rn>=cn && re>=ce && x*(cb*sb + cn*sn + ce*se) >= v)
                >>= (\(cb,cn,ce) -> min (x*(cb*sb + cn*sn + ce*se)) <$> speed' (rb-cb,rn-cn,re-ce) xs)

        binary :: Int -> Int -> Maybe Int
        binary lo hi = case speed mi of
                Nothing             -> binary lo (mi-1)
                Just  s | lo+1==hi  -> max (Just s) (speed hi)
                        | lo >= hi  -> Just s
                        | otherwise -> max (Just s) (binary (s+1) hi)
          where
            mi = lo + (hi-lo) `div` 2

    maximum ks * (2*se) + 1
        & binary 0
        & fromMaybe 0
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
