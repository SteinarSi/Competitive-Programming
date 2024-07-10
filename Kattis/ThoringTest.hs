import           Control.Arrow            ((>>>), (&&&))
import           Data.Bool                (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char                (toLower)
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.map toLower
        >>> C.lines
        >>> tail
        >>> (init >>> S.fromList >>> flip S.member) 
                &&& 
            (last >>> C.words)
        >>> uncurry all
        >>> bool "Thore has left the chat" "Hi, how do I look today?"
        >>> putStrLn
    )
