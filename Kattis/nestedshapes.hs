import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

data Shape = C | S | D
    deriving Read

main :: IO ()
main = do
    [_, xs, ap, size, q] <- getContents <&> words

    let start:shapes = map (pure >>> read) xs

        s = read size :: Double
        startRadius = case (ap,start) of
            ("A",C) -> sqrt (s / pi)
            ("A",S) -> sqrt s / 2
            ("A",D) -> sqrt s / 2
            ("P",C) -> (s / pi) / 2
            ("P",S) -> s / 8
            ("P",D) -> s / 8

        (endRadius,last) = nested (startRadius,start) shapes

    print $ case (q,last) of
        ("A",C) -> endRadius^2 * pi
        ("A",_) -> (2*endRadius)^2
        ("P",C) -> (2*endRadius) * pi
        ("P",_) -> 8*endRadius

nested :: (Double,Shape) -> [Shape] -> (Double,Shape)
nested (r,s) [] = (r,s)
nested (r,s) (x:xs) = nested (r',x) xs
  where
    r' = case (s,x) of
        (C,S) -> r / sqrt 2
        (C,D) -> r / sqrt 2
        (S,D) -> r / sqrt 2
        (D,S) -> r / sqrt 2
        (S,C) -> r
        (D,C) -> r
