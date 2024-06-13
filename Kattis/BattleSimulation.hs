import           Control.Arrow ((>>>))
import           Data.List     (sort)

main :: IO ()
main = getContents >>= (
            insertComboBreakers
        >>> insertCounters
        >>> putStrLn
    )

insertComboBreakers :: String -> String
insertComboBreakers "" = ""
insertComboBreakers (x:y:z:xs) | sort [x,y,z] == "BLR" = 'C' : insertComboBreakers xs
insertComboBreakers (x:xs) = x : insertComboBreakers xs

insertCounters :: String -> String
insertCounters ""       = ""
insertCounters ('R':xs) = 'S' : insertCounters xs
insertCounters ('B':xs) = 'K' : insertCounters xs
insertCounters ('L':xs) = 'H' : insertCounters xs
insertCounters ('C':xs) = 'C' : insertCounters xs
insertCounters (_:xs)   = insertCounters xs
