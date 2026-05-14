import           System.IO   (hFlush, stdout)
import           Text.Printf (printf)

main :: IO ()
main = solve 0 1152921504606846975

solve :: Int -> Int -> IO ()
solve lo hi = do
    let mi = (lo+hi) `div` 2
    printf "? %.15X\n" mi
    hFlush stdout
    res <- getLine
    case res of
        "Too high!"       -> solve lo (mi-1)
        "Too low!"        -> solve (mi+1) hi
        "That's correct!" -> pure ()
