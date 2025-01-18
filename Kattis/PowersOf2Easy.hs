import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [n,e] <- getContents <&> (words >>> map read)

    let two = 2^e
        too = two
            & show
            & C.pack
            & C.isInfixOf
    
    [two..n]
        & filter (show >>> C.pack >>> too)
        & length
        & print
