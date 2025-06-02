module Main ( main
            ) where

import           System.Environment
import           Data.Map (Map)
import qualified Data.Map as Map
import           Parser
import           Tape
import           TuringMachine



main :: IO ()
main = do
    args <- getArgs
    case args of
        [path, tape] -> runTM path tape
        _ -> putStrLn "Usage: tm <path> <tape>"

runTM :: String -> String -> IO ()
runTM path strTape = do
    tms <- parseFile path
    case tms of
        Left err -> putStrLn err
        Right tms' -> execTMs tms' strTape

execTMs :: Map String TM -> String -> IO ()
execTMs tms strTape = putStrLn $ showTape tape tapeHead
    where
        (tape, tapeHead) = performCall tms Map.empty (tapeFromStr strTape) initTapeHead ("Main", [])
