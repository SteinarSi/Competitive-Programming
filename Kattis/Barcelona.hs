import Data.Functor ((<&>))
import Data.List (elemIndex)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    _:bag:bags <- C.getContents <&> C.words
    putStrLn $ case elemIndex bag bags of
        Just 0 -> "fyrst"
        Just 1 -> "naestfyrst"
        Just x -> show (x + 1) ++ " fyrst"

