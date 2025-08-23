import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Functor  ((<&>))
import           Data.List     ((\\))

main :: IO ()
main = getContents >>= (
            lines
        >>> tail
        >>> (["keys", "phone", "wallet"] \\)
        >>> (\xs -> bool xs ["ready"] (null xs))
        >>> mapM_ putStrLn
    )
