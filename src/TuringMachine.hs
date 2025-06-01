module TuringMachine ( simulate
                     , TMGraph
                     ) where

import           Tape
import           TMState
import           Data.List



type TMGraph = [((NodeState, Symbol), (NodeState, Symbol, Move))]

firstMatch :: NodeState -> Symbol -> TMGraph -> Maybe (NodeState, Symbol, Move)
firstMatch _ _ [] = Nothing
firstMatch state _ (((state', WildCard), assoc) : _)
    | state == state' = Just assoc
firstMatch state symb (((state', symb'), assoc) : _)
    | state == state' && symb == symb' = Just assoc
firstMatch state symb (_ : xs) = firstMatch state symb xs

step :: TMGraph -> Tape -> TapeState -> Maybe (Tape, TapeState)
step graph tape tapeState = next <$> transition
    where
        currHead = headPos tapeState
        currSymbol = symbolAt (headPos tapeState) tape
        transition = firstMatch (currState tapeState) currSymbol graph
        next (nodeState, symb, m) = (tape', state')
            where
                tape' = placeSymbol currHead symb tape
                state' = TapeState (move currHead m) nodeState

simulate :: TMGraph -> Tape -> TapeState -> (Tape, TapeState)
simulate graph tape state = last $ (tape, state) : unfoldr unfolder (tape, state)
    where
        unfolder (t, s) = (\x -> (x, x)) <$> step graph t s
