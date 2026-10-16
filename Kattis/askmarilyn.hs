import qualified Data.ByteString.Char8 as C
import           System.IO             (hFlush, stdout)

main :: IO ()
main = play 1000 generation

play :: Int -> [Char] -> IO ()
play 0 _      = pure ()
play n (x:xs) = do
    putStrLn [x]
    hFlush stdout
    response <- C.getLine
    putStrLn $ if C.last response == '1'
        then [C.head response]
        else (filter (\y -> y /= x && y /= C.head response) "ABC")
    hFlush stdout
    C.getLine
    play (n-1) xs

generation :: [Char]
generation = cycle "BACBACBAACCABACBBCABBACCABBAC"
