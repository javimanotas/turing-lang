module Parser ( tmFromFile
              ) where

import           TuringMachine
import           Data.Map (Map)
import qualified Data.Map as Map
import           Text.Parsec hiding (Empty)
import           Control.Monad
import           TMState
import           Tape hiding (move)



tmFromFile :: FilePath -> IO (Either String (Map String TMGraph))
tmFromFile path = do
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

tms :: StrParser (Map String TMGraph)
tms = Map.fromList <$> many1 tm

tm :: StrParser (String, TMGraph)
tm = do
    c <- upper
    rest <- many letter
    let name = c:rest
    transitions <- between (char '(') (char ')') $ many transition
    return (name, transitions)

nodeState :: StrParser NodeState
nodeState = char 'q' >> Q . read <$> many1 digit

ignoreThen :: Monad m => m a -> b -> m b
ignoreThen parser x = parser >> return x

symbol :: StrParser Symbol
symbol = empt <|> sChar <|> wildCard
    where
        empt = char 'B' `ignoreThen` Empty
        wildCard = char '_' `ignoreThen` WildCard
        sChar = Symbol <$> (upper <|> digit)

move :: StrParser Move
move = choice [ char 'L' `ignoreThen` L
              , char 'S' `ignoreThen` S
              , char 'R' `ignoreThen` R ]

transition :: StrParser ((NodeState, Symbol), (NodeState, Symbol, Move))
transition = do
    void $ char '('
    st0 <- nodeState
    void $ char ','
    symb0 <- symbol
    void $ string ")->("
    st1 <- nodeState
    void $ char ','
    symb1 <- symbol
    void $ char ','
    m <- move
    void $ char ')'
    return ((st0, symb0), (st1, symb1, m))
