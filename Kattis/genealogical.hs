import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (intercalate, sort)
import qualified Data.Map.Strict       as M

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.unpack >>> parse)
        >>> interpret M.empty M.empty M.empty M.empty
        >>> intercalate "\n"
        >>> putStr
    )

interpret :: M.Map String String
    -> M.Map String String
    -> M.Map String [String]
    -> M.Map String [String]
    -> [Command] -> [String]
interpret births deaths parents children [] = []
interpret births deaths parents children (x:xs) = case x of
    BIRTH name date mum dad -> interpret
        (M.insert name date births)
        deaths
        (M.insert name [mum,dad] parents)
        (M.insertWith (<>) mum [name] (M.insertWith (<>) dad [name] children))
        xs
    DEATH name date -> interpret births (M.insert name date deaths) parents children xs
    ANCESTORS name -> unlines ("ANCESTORS of " <> name : drop 1 (formatRelatives parents name)) : interpret births deaths parents children xs
    DESCENDANTS name -> unlines ("DESCENDANTS of " <> name : drop 1 (formatRelatives children name)) : interpret births deaths parents children xs
  where
    formatName :: String -> String
    formatName name = maybe name (\b -> name <> " " <> b <> " -" <> maybe "" (' ':) (M.lookup name deaths)) (M.lookup name births)

    formatRelatives :: M.Map String [String] -> String -> [String]
    formatRelatives rels name = M.findWithDefault [] name rels
        & sort
        & concatMap (formatRelatives rels >>> map indent)
        & (formatName name :)

indent :: String -> String
indent = ("  " <>)

data Command =
      BIRTH String String String String
    | DEATH String String
    | ANCESTORS String
    | DESCENDANTS String
  deriving Show

parse :: String -> Command
parse ('A':'N':'C':'E':'S':'T':'O':'R':'S':_:name) = ANCESTORS name
parse ('D':'E':'S':'C':'E':'N':'D':'A':'N':'T':'S':_:name) = DESCENDANTS name
parse ('B':'I':'R':'T':'H':_:xs) = BIRTH name date mum dad
  where
    [name,date,mum,dad] = split "" xs
parse ('D':'E':'A':'T':'H':_:xs) = DEATH name date
  where
    [name,date] = split "" xs

split :: String -> String -> [String]
split "" ""                = []
split ret ""               = [reverse ret]
split ret (' ':':':' ':xs) = reverse ret : split "" xs
split ret (x:xs)           = split (x:ret) xs
