module TMState ( Symbol (..)
               , symbolFromChar
               , NodeState (..)
               , initNodeState
               ) where



data Symbol = Empty
            | Symbol Char
            | WildCard
            deriving (Eq, Ord)

-- this is a partial implementation
-- wildcards are only used for checking pattern matching,
-- therefore they will never be shown
instance Show Symbol where
    show Empty = "B"
    show (Symbol s) = [s]
    show WildCard = error "cannot show wildcard"

-- note that this function never returns a wildcard
-- wildcards should ONLY be used when defining a tm not the tape's content
symbolFromChar :: Char -> Symbol
symbolFromChar 'B' = Empty
symbolFromChar x   = Symbol x



newtype NodeState = Q Int
                  deriving (Eq, Ord)

instance Show NodeState where
    show (Q i) = 'q' : show i

initNodeState :: NodeState
initNodeState = Q 0
