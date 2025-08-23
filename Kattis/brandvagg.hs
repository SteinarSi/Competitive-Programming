{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (first, second, (&&&), (>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toUpper)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

data Action = ACCEPT
            | LOG
            | DROP
    deriving (Show,Read)

data Condition = PORT Int
               | IP C.ByteString
               | LIMIT Int
    deriving Show

type Packet = (C.ByteString,Int)
type Rule = (Action,[Condition])

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines

    let (rules,p':rest') = splitAt (readInt n) rest
            & first (map parseRule)
        p = readInt p'
        packets = listArray (1,p) (map parsePacket rest')

    firewall p packets rules M.empty 1
        & unlines
        & putStr

firewall :: Int -> Array Int Packet -> [Rule] -> M.Map C.ByteString Int -> Int -> [String]
firewall p packets rules count i | i > p     = []
                                 | otherwise = handle rules <> firewall p packets rules count' (i+1)
  where
    (ip,port) = packets ! i
    count' | i > 1000  = M.insertWith (+) ip 1 (M.adjust pred (fst (packets ! (i-1000))) count)
           | otherwise = M.insertWith (+) ip 1 count

    handle :: [Rule] -> [String]
    handle [] = []
    handle ((a,cs):xs) | not (all match cs)  = handle xs
                       | otherwise = case a of
                            ACCEPT -> ["accept " <> show i]
                            LOG    -> ("log "    <> show i) : handle xs
                            DROP   -> ["drop "   <> show i]

    match :: Condition -> Bool
    match (PORT p ) = p == port
    match (IP i   ) = i == ip
    match (LIMIT l) = count' M.! ip >= l

parsePacket :: C.ByteString -> Packet
parsePacket = C.split ':' >>> head &&& (last >>> readInt)

parseCondition :: C.ByteString -> Condition
parseCondition xs = case C.split '=' xs of
    ["port" ,port ] -> PORT  (readInt port)
    ["ip"   ,ip   ] -> IP    ip
    ["limit",limit] -> LIMIT (readInt limit)

parseRule :: C.ByteString -> Rule
parseRule = C.words
    >>> (head >>> C.unpack >>> map toUpper >>> read)
            &&&
        (tail >>> map parseCondition)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
