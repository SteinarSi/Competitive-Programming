import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    xs <- C.getContents <&> (C.words >>> drop 1)
    let woke = length (filter (C.count '-' >>> (>1)) xs)
        n = length xs
        per = (100*woke+n-1) `div` n
    putStrLn ("Þessi texti er " <> show per <> "% woke.")
