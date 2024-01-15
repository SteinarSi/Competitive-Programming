import           Data.List (sort)
import           Data.Map  (Map, elems, empty, insertWith)

main :: IO ()
main = do
    friends <- fmap (map words . tail . lines) getContents

    let memo = foldr (\[name,rating,bday] -> insertWith max bday (read rating :: Int, name)) empty friends
        realFriends = sort . map snd $ elems memo

    print (length realFriends)
    mapM_ putStrLn realFriends
