module TMState ( Symbol (..)
               , symbolFromChar
               , NodeState (..)
               , initNodeState
               ) where



data Symbol = Empty
            | Symbol Char
            deriving (Eq, Ord)

instance Show Symbol where
    show Empty = "B"
    show (Symbol s) = [s]

symbolFromChar :: Char -> Symbol
symbolFromChar 'B' = Empty
symbolFromChar x   = Symbol x



newtype NodeState = Q Int
                  deriving (Eq, Ord)

instance Show NodeState where
    show (Q i) = 'q' : show i

initNodeState :: NodeState
initNodeState = Q 0
