import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (delete)

data Card = Card {
    cost       :: Int,
    damage     :: Int,
    vulnerable :: Bool,
    gain       :: Int,
    hits       :: Int
}   deriving Eq

main :: IO ()
main = do
    [e]:[v,s,a]:xs <- getContents <&> (lines >>> map (words >>> map read))

    let (vs,sa) = splitAt v xs
        (ss,as) = splitAt s sa
        cards = map (\[c,d] -> Card c d True 0 1) vs
            <> map (\[c,d,s] -> Card c d False s 1) ss
            <> map (\[c,d,h] -> Card c d False 0 h) as

    print (plai cards (e,False,0))

plai :: [Card] -> (Int,Bool,Int) -> Int
plai _ (e,_,_) | e < 0 = minBound
plai xs (e,v,s) = xs
        & filter (cost >>> (<=e))
        & map (\k@Card { cost, damage, vulnerable, gain, hits } -> 
            ceiling (fromIntegral (damage+s) * bool 1 1.5 v) * hits + plai (delete k xs) (e - cost, v || vulnerable, s + gain)
            )
        & (0:)
        & maximum
