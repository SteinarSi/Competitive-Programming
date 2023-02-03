import Control.Monad (replicateM)

main :: IO ()
main = fmap read getLine >>= \n -> replicateM n getLine >>= print . (1 - n +) . sum . map read