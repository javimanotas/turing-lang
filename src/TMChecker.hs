module TMChecker ( check
                 ) where

import qualified Data.Map as Map
import           Data.Map (Map)
import           TuringMachine
import           Parser
import           Data.List
import           Data.Maybe



check :: ParsedOutput -> Either [String] (Map String TM)
check output = case duplicatedTMS output of
    [] -> case foldMap ($ tmsMap) checkers of
            [] -> Right tmsMap
            l -> Left l
    l -> Left l
    where
        checkers = [ duplicatedArgs
                   , wrongNumberOfArgs
                   , mainNotFound
                   , mainHasArgs
                   ]
        tmsMap = Map.fromList output



duplicatedTMS :: ParsedOutput -> [String]
duplicatedTMS l = map (\c -> "Found " ++ show (length c) ++ " defitions for " ++ head c ++ " (only one is allowed)") namesChunks
    where
        namesChunks = filter ((/=1) . length) $ group $ sort $ map fst l

duplicatedArgs :: Map String TM -> [String]
duplicatedArgs l = map (\(n, _) -> "Found duplicated arguments for " ++ n)
                 $ filter (\(_, t) -> hasDuplicated $ params t)
                 $ Map.toList l
    where
        hasDuplicated l' = length (nub l') < length l'

wrongNumberOfArgs :: Map String TM -> [String]
wrongNumberOfArgs l = [ fst c ++ " was called but is not defined or its defined with another number of arguments"
                      | c <- foldMap callsOf $ Map.keys l
                      , isWrong c ]
    where
        isWrong (n, a) = n `Map.notMember` l || length a /= length (params $ l Map.! n)
        callsOf :: String -> [TMCall]
        callsOf str = case body $ l Map.! str of
            Graph _ -> []
            Seq c b -> c ++ map snd b

mainNotFound :: Map String TM -> [String]
mainNotFound l
    | isNothing (Map.lookup "Main" l) = ["Main not found"]
    | otherwise = []

mainHasArgs :: Map String TM -> [String]
mainHasArgs l
    | nParams == 0 = []
    | otherwise = ["Found definition of Main with " ++ show nParams ++ " args (expected 0)"]
    where
        nParams = maybe 0 (length . params) $ Map.lookup "Main" l
