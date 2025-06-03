module Main ( main
            ) where

import           System.Environment
import           Data.Map (Map)
import qualified Data.Map as Map
import           Parser
import           Tape
import           TuringMachine
import           GHC.IO.Handle.Text
import           System.IO
import           System.Exit
import           TMChecker



main :: IO ()
main = do
    args <- getArgs
    case args of
        [path, tape] -> runTM path tape
        _ -> printErrs ["Usage: cabal run turing-lang -- <path-to-machine-file> <initial-tape>"]

runTM :: String -> String -> IO ()
runTM path strTape = do
    tms <- parseFile path
    case tms of
        Left err -> printErrs [err]
        Right tms' -> case check tms' of
                    Left l -> printErrs $ map ("Error: "++) l
                    Right tms'' -> execTMs tms'' strTape                

execTMs :: Map String TM -> String -> IO ()
execTMs tms strTape = putStrLn $ showTape tape tapeHead
    where
        (tape, tapeHead) = performCall tms Map.empty (tapeFromStr strTape) initTapeHead ("Main", [])

printErrs :: [String] -> IO a
printErrs errs = do
    mapM_ (hPutStrLn stderr) errs
    exitFailure
