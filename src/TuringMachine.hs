module TuringMachine ( simulate
                     , TMGraph
                     ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Tape
import           TMState
import           Data.List



type TMGraph = Map (NodeState, Symbol) (NodeState, Symbol, Move)

step :: TMGraph -> Tape -> TapeState -> Maybe (Tape, TapeState)
step graph tape tapeState = next <$> transition
    where
        currHead = headPos tapeState
        currSymbol = symbolAt (headPos tapeState) tape
        transition = Map.lookup (currState tapeState, currSymbol) graph
        next (nodeState, symb, m) = (tape', state')
            where
                tape' = placeSymbol currHead symb tape
                state' = TapeState (move currHead m) nodeState

simulate :: TMGraph -> Tape -> TapeState -> (Tape, TapeState)
simulate graph tape state = last $ (tape, state) : unfoldr unfolder (tape, state)
    where
        unfolder (t, s) = (\x -> (x, x)) <$> step graph t s
