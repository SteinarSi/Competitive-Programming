import           Control.Monad (when)
import           Data.Set      (Set, delete, empty, insert, member, notMember)

main :: IO ()
main = getLine >> getContents >>= secure empty . map words . lines

secure :: Set String -> [[String]] -> IO ()
secure _ [] = pure ()
secure inside (["entry", name]:xs) = do
    putStr (name ++ " entered")
    when (member name inside) (putStr " (ANOMALY)")
    putChar '\n'
    secure (insert name inside) xs
secure inside (["exit", name]:xs) = do
    putStr (name ++ " exited")
    when (notMember name inside) (putStr " (ANOMALY)")
    putChar '\n'
    secure (delete name inside) xs
