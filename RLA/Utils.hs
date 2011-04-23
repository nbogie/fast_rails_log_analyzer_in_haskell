module RLA.Utils where

import RLA.Parser
import RLA.Types

import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy.Char8 as C

linesOfFile :: String -> IO [C.ByteString]
linesOfFile fname = do c <- C.readFile fname
                       return $ C.lines c

parseLogFile :: String -> IO [LogEvent]
parseLogFile fname = do
  ls <- linesOfFile fname
  return $ mapMaybe parseLogEvent ls
