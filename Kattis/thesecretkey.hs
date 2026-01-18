import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (find)
import qualified Data.Set      as S
import           Text.Printf   (printf)

main :: IO ()
main = do
    n:xs <- getContents <&> lines
    let Just x = [0::Int ..]
            & map (printf "%0*b" (read n :: Int))
            & find (`S.notMember` S.fromList xs)
    putStrLn x
