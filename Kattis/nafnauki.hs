import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.interact (C.takeWhileEnd (/='.') >>> C.cons '.')
