module Tape ( Tape
            , symbolAt
            , placeSymbol
            , TapeHead
            , initTapeHead
            , TapeState (..)
            , tapeFromStr
            , showTape
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



data Move = L | S | R
          deriving (Eq, Show, Ord)

move :: TapeHead -> Move -> TapeHead
move (TapeHead n) L = TapeHead $ n - 1
move (TapeHead n) S = TapeHead n
move (TapeHead n) R = TapeHead $ n + 1



newtype Tape = Tape { getTape :: Map TapeHead Char }

showTape :: Tape -> TapeHead -> String
showTape (Tape t) _
    | t == Map.empty = show Empty
showTape t hd = foldMap (show . (\i -> symbolAt (TapeHead i) t)) [getHead hd .. lst + 1]
    where
        lst = getHead $ fst $ Map.findMax $ getTape t

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
