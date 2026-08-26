import           Control.Arrow         ((>>>))
import           Data.Array.Base       (elems)
import           Data.Array.ST         (newArray, readArray, runSTUArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))

main :: IO ()
main = do
    xs <- C.getLine

    let cnt = runSTUArray $ do
            ret <- newArray ('a','z') (0::Int)
            C.foldr (\x -> (readArray ret x >>= (succ >>> writeArray ret x) >>)) (pure ()) xs
            pure ret

    elems cnt
        & filter odd
        & length
        & (<=1)
        & bool "no" "yes"
        & putStrLn
