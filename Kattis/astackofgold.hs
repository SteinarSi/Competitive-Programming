import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [w, s] <- getContents <&> (words >>> map read)

    let expected = tungsten * s*(s+1) `div` 2
        surplus = w - expected
        golds = surplus `div` (gold-tungsten)

    print golds

tungsten :: Int
tungsten = 29260

gold :: Int
gold = 29370
