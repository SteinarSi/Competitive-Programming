import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as B
import           Data.Functor          ((<&>))
import           Data.List             (foldl')
import           Data.Map              (Map, assocs, empty, insertWith)

main :: IO ()
main = do
    xs <- B.getContents <&> (B.lines >>> init >>> foldl' (\m x -> insertWith (+) x 1 m) empty >>> assocs)
    let best = maximum (map snd xs)
    B.putStrLn $ case filter (snd >>> (==best)) xs of
        []      -> error "bruh"
        [x]     -> fst x
        (_:_:_) -> B.pack "Runoff!"
