module Parser ( parseFile
              ) where

import           TuringMachine
import           Data.Map (Map)
import qualified Data.Map as Map
import           Text.Parsec hiding (Empty)
import           Control.Monad
import           TMState
import           Tape hiding (move)



parseFile :: FilePath -> IO (Either String (Map String TM))
parseFile path = do
    fLines <- lines <$> readFile path
    let withoutComments = map removeComments fLines
    let stream = filter (/= ' ') $ unwords withoutComments
    return $ case parse (tms <* eof) "" stream of
        Left err -> Left $ show err
        Right tmsMap -> Right tmsMap

removeComments :: String -> String
removeComments ""     = ""
removeComments ('-':'-':_)   = ""
removeComments (x:xs) = x : removeComments xs



type StrParser a = Parsec String () a



tms :: StrParser (Map String TM)
tms = Map.fromList <$> many tm

tm :: StrParser (String, TM)
tm = do
    name <- ident
    prms <- many (char '_' >> var)
    void $ char '='
    bdy <- between (char '(') (char ')') tmBody
    return (name, TM prms bdy)

tmBody :: StrParser TMBody
tmBody = try tmSeq <|> try seqCalls <|> try seqBranches <|> trans
    where
        trans = Graph <$> many transition



ignoreThen :: Monad m => m a -> b -> m b
ignoreThen parser x = parser >> return x

ident :: StrParser String
ident = do
    c <- upper
    rest <- many letter
    return $ c : rest

var :: StrParser String
var = many1 lower

nodeState :: StrParser NodeState
nodeState = char 'q' >> Q . read <$> many1 digit

patt :: StrParser Pattern
patt = empt <|> sChar <|> wildCard <|> Var <$> var
    where
        empt = char 'B' `ignoreThen` Symb Empty
        sChar = Symb . Symbol <$> (upper <|> digit)
        wildCard = char '_' `ignoreThen` WildCard

move :: StrParser Move
move = choice [ char 'L' `ignoreThen` L
              , char 'S' `ignoreThen` S
              , char 'R' `ignoreThen` R ]



tmSeq :: StrParser TMBody
tmSeq = do
    clls <- tmCall `sepBy1` char ','
    brnchs <- char '|'  >> branch `sepBy` char '|'
    return (Seq clls brnchs)

seqCalls :: StrParser TMBody
seqCalls = do
    clls <- tmCall `sepBy1` char ','
    return (Seq clls [])

seqBranches :: StrParser TMBody
seqBranches = do
    brnchs <- char '|' >> branch `sepBy1` char '|'
    return (Seq [] brnchs)

branch :: StrParser (Pattern, TMCall)
branch = do
    p <- patt
    void $ string "->"
    cll <- tmCall
    return (p, cll)

tmCall :: StrParser TMCall
tmCall = do
    name <- ident
    patterns <- many (char '_' >> patt)
    return (name, patterns)



transition :: StrParser TMGraphEdge
transition = do
    void $ char '('
    st0 <- nodeState
    void $ char ','
    p0 <- patt
    void $ string ")->("
    st1 <- nodeState
    void $ char ','
    p1 <- patt
    void $ char ','
    m <- move
    void $ char ')'
    return ((st0, p0), (st1, p1, m))
