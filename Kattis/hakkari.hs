import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Ix               (range)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m] <- C.getLine <&> (C.words >>> map readInt)
    C.getContents >>= (
                C.filter (`elem` ".*")
            >>> C.unpack
            >>> zip (range ((1,1), (n,m)))
            >>> filter (snd >>> (=='*'))
            >>> map (\((y,x),_) -> C.pack (show y <> " " <> show x))
            >>> ((length >>> show >>> C.pack) &&& id)
            >>> uncurry (:)
            >>> C.unlines
            >>> C.putStr
        )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
