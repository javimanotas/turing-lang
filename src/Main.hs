module Main where

import           System.Environment
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
    tm <- tmFromFile path
    case tm of
        Left err -> putStrLn err
        Right tms -> case Map.lookup "Main" tms of
            Nothing -> putStrLn "Error: Main not found"
            Just tm' -> let tape = tapeFromStr strTape
                            (result, _) = simulate tm' tape initTapeState 
                        in print result
