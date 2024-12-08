import           Data.Bool   (bool)
import           Text.Printf (printf)

main :: IO ()
main = do
    n <- fmap read getContents :: IO Int
    printf "%d ml gin\n%d ml fresh lemon juice\n%d ml simple syrup\n%d slice%s of lemon\n" (n*45) (n*30) (n*10) n (bool "" "s" (n>1))
