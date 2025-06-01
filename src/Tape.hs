module Tape ( Tape
            , symbolAt
            , placeSymbol
            , TapeState (..)
            , initTapeState
            , tapeFromStr
            , Move (..)
            , move
            ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           TMState



newtype TapeHead = TapeHead { getHead :: Int }
                 deriving (Eq, Ord, Show)

initTapeHead :: TapeHead
initTapeHead = TapeHead 0



data TapeState = TapeState { headPos   :: TapeHead
                           , currState :: NodeState
                           } deriving (Eq, Ord)

initTapeState :: TapeState
initTapeState = TapeState initTapeHead initNodeState



data Move = L | S | R
          deriving (Eq, Show, Ord)

move :: TapeHead -> Move -> TapeHead
move (TapeHead n) L = TapeHead $ n - 1
move (TapeHead n) S = TapeHead n
move (TapeHead n) R = TapeHead $ n + 1



newtype Tape = Tape { getTape :: Map TapeHead Char }

instance Show Tape where
    show t = foldMap (show . (\i -> symbolAt (TapeHead i) t)) [first - 1 .. lst + 1]
        where
            first = getHead $ fst $ Map.findMin $ getTape t
            lst   = getHead $ fst $ Map.findMax $ getTape t


tapeFromStr :: String -> Tape
tapeFromStr str = Tape $ foldl insert Map.empty indexed
    where
        indexed = filter ((/= 'B') . snd) $ zip [0..] str
        insert m (i, s) = Map.insert (TapeHead i) s m
        

symbolAt :: TapeHead -> Tape -> Symbol
symbolAt idx (Tape t) = maybe Empty Symbol (Map.lookup idx t)

placeSymbol :: TapeHead -> Symbol -> Tape -> Tape
placeSymbol tapeHead Empty      (Tape tape) = Tape $ Map.delete tapeHead tape
placeSymbol tapeHead (Symbol s) (Tape tape) = Tape $ Map.insert tapeHead s tape
