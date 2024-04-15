import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ ((C.pack "Takk " <>) >>> C.putStrLn)
    )
