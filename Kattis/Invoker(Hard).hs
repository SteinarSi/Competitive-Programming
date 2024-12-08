import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

data Quat = Quat {
        a :: Double,
        b :: Double,
        c :: Double,
        d :: Double
    }

main :: IO ()
main = do
    [p,q] <- getContents <&> (lines >>> map parse)
    print (p >< reciprocal q)

parse :: String -> Quat
parse xs = let [a,b,c,d] = map read (words xs)
           in  Quat {a, b, c, d}

(><) :: Quat -> Quat -> Quat
(><) p q = Quat {
        a = a p * a q - b p * b q - c p * c q - d p * d q,
        b = a p * b q + b p * a q + c p * d q - d p * c q,
        c = a p * c q - b p * d q + c p * a q + d p * b q,
        d = a p * d q + b p * c q - c p * b q + d p * a q
    }

reciprocal :: Quat -> Quat
reciprocal q = let Quat {a,b,c,d} = conjugate q
                   scale = norm q ^ 2
               in  Quat {
                    a = a / scale,
                    b = b / scale,
                    c = c / scale,
                    d = d / scale
                }

conjugate :: Quat -> Quat
conjugate Quat {a,b,c,d} = Quat {
        a = a,
        b = -b,
        c = -c,
        d = -d
    }

norm :: Quat -> Double
norm Quat {a,b,c,d} = sqrt (a^2 + b^2 + c^2 + d^2)

instance Show Quat where
    show (Quat {a, b, c, d}) = unwords (map show [a,b,c,d])
