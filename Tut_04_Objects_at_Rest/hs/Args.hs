module Args where

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, IOException)

parseArgs :: IO (String)
parseArgs = do
    filename <- catch extract handle
    return filename
    where
        extract :: IO (String)
        extract = do
            [filename] <- getArgs
            return filename
        handle :: IOException -> IO String
        handle e = do
            n <- getProgName
            hPutStrLn stderr $ "USAGE: " ++ n ++ " filename"
            exitFailure

