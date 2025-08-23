import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as C
import           Control.Arrow ((>>>), (&&&))

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> mapM_ (
                (C.length >>> fromIntegral >>> fac)
                    &&& 
                (C.foldr (flip (M.insertWith (+)) 1) M.empty >>> M.elems >>> map fac >>> product)
            >>> uncurry div
            >>> show
            >>> C.pack
            >>> C.putStrLn
        )
    )

fac :: Integer -> Integer
fac = enumFromTo 2 >>> product
