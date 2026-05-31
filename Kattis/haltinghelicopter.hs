import           Control.Arrow ((>>>))
import           Data.Bits     (shiftL)
import           Data.Functor  ((<&>))
import           Data.List     (find)

main :: IO ()
main = do
    n':xs <- getContents <&> (words >>> map read)

    let n = round n'
        cs = init xs
        cn = last xs

        f t  = sum (zipWith (\c i -> c*t^^i) cs [1..]) - cn * t^n
        f' t = sum (zipWith (\c i -> fromIntegral i * c*t^^(i-1)) cs [1..]) - n' * cn * t^(n-1)

        Just mt = find (f >>> (<0)) (map ((1 `shiftL`) >>> fromInteger) [1..])

        bin :: Int -> Double -> Double -> Double
        bin 0 lo hi = (lo + hi) / 2
        bin i lo hi = let mi = (lo + hi) / 2
                    in  case compare (f' mi) 0 of
                            LT -> bin (i-1) lo mi
                            EQ -> mi
                            GT -> bin (i-1) mi hi

    print (bin 100 0 mt)
