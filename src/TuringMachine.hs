module TuringMachine ( Pattern (..)
                     , TMCall
                     , TMGraphEdge
                     , TMBody (..)
                     , TM (..)
                     , performCall
                     ) where

import           Tape
import           TMState
import qualified Data.Map as Map
import           Data.Map (Map)



data Pattern = Symb Symbol
             | Var String
             | WildCard
             deriving (Eq, Show)



type Args = Map String Symbol

type TMGraphEdge = ((NodeState, Pattern), (NodeState, Pattern, Move))

type TMCall = (String, [Pattern])



data TMBody = Graph [TMGraphEdge]
            | Seq { calls :: [TMCall]
                  , branches :: [(Pattern, TMCall)]
                  } deriving (Show)

data TM = TM { params :: [String]
             , body :: TMBody
             } deriving (Show)



matches :: Args -> Symbol -> Pattern -> Bool
matches _ _ WildCard = True
matches _ s (Symb s') = s == s'
matches a s (Var v) = a Map.! v == s



firstMatch :: Args -> NodeState -> Symbol -> [TMGraphEdge] -> Maybe (NodeState, Pattern, Move)
firstMatch _    _     _    [] = Nothing
firstMatch args state symb (((state', pattern), x) : _)
    | state == state' && matches args symb pattern = Just x
firstMatch args state symb (_:xs) = firstMatch args state symb xs



resolvePattern :: Args -> Tape -> TapeHead -> Pattern -> Symbol
resolvePattern _ _ _ (Symb s) = s
resolvePattern _ t h WildCard = symbolAt h t
resolvePattern a _ _ (Var v) = a Map.! v



step :: Args -> [TMGraphEdge] -> Tape -> TapeState -> Maybe (Tape, TapeState)
step args graph tape tapeState = next <$> transition
    where
        currHead = headPos tapeState
        currSymbol = symbolAt (headPos tapeState) tape
        transition = firstMatch args (currState tapeState) currSymbol graph

        next :: (NodeState, Pattern, Move) -> (Tape, TapeState)
        next (node, pattern, m) = (tape', state')
            where
                symb = resolvePattern args tape (headPos tapeState) pattern
                tape' = placeSymbol currHead symb tape
                state' = TapeState (move currHead m) node



performCall :: Map String TM -> Args -> Tape -> TapeHead -> TMCall -> (Tape, TapeHead)
performCall tms args tape hd (name, patterns) = simulate tms tm args' tape hd
    where
        tm = tms Map.! name
        patts = map (resolvePattern args tape hd) patterns
        args' = Map.fromList $ zip (params tm) patts



branch :: Map String TM -> Args -> [(Pattern, TMCall)] -> (Tape, TapeHead) -> (Tape, TapeHead)
branch _ _ [] x = x
branch tms args ((pattern, call):xs) (tape, hd)
    | matches args currSymb pattern = performCall tms args tape hd call
    | otherwise = branch tms args xs (tape, hd)
    where
        currSymb = symbolAt hd tape



simulate :: Map String TM -> TM -> Args -> Tape -> TapeHead -> (Tape, TapeHead)
simulate tms tm args tape hd = case body tm of
    Graph g -> let (t, s) = untilNothing (uncurry $ step args g) (tape, TapeState hd initNodeState)
               in (t, headPos s)
    Seq cs b -> branch tms args b 
                $ foldl (uncurry $ performCall tms args) (tape, hd) cs
    where
        untilNothing f a = case f a of
            Nothing -> a
            Just b -> untilNothing f b
