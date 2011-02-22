module Parser 
  (
    extractCore
  , extractAction
  , extractDuration
  , parseLogEvent
  ) where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Regex.Posix
-- import Data.List (isInfixOf)
-- import Text.ParserCombinators.Parsec
-- import Data.Time.Parse
-- import Data.Time.LocalTime

import Types

-- we may change this to extractCoreWithParsec, or other impl
extractCore = extractCoreWithRead

-- "Feb 10 06:53:40 host1 rails[28275]: local3.info<158>: Processing ..."
--  ^--timestamp--^ ^host       ^pid^   ^-sev/priority/-^ ^--message---...   
extractCoreWithRead :: SomeString -> Maybe (Timestamp, Hostname, Pid, SomeString)
extractCoreWithRead input = 
      let timestamp=C.take timestampWidth input
          (hostname:pidPlus:_severity:firstWord:_rest) = C.words (C.drop (timestampWidth+1) input)
          -- (pidStr, _afterPid)  = (C.break (== ']') (C.drop 14 pidPlus)) -- don't hardcode the length of the rails process
          (pidStr, _afterPid) = C.break (==']') $ C.drop 1 $ snd $ C.break (=='[') pidPlus -- yuck FIXME
      in case C.readInt pidStr of
           Just (pid, _) -> Just (C.copy timestamp, C.copy hostname, pid, firstWord)
           Nothing -> Nothing


-- extract an action and an optional format.  may fail entirely, returning Nothing.
-- The action will have its own copies of strings, not pointers into the bytestring.
extractAction = extractActionFast
-- TODO: handle case where the action cannot be parsed
extractActionFast :: SomeString -> Maybe (SomeString, Maybe SomeString)
extractActionFast inputBS = let (action:w2:w3:[]) = take 3 (drop 4 (C.words (C.drop (timestampWidth+1) inputBS)))
                                format = if w2 == C.pack "to" then Just w3 else Nothing
                            in Just (C.copy action, fmap C.copy format)


extractDuration = extractDurationFast

extractDurationFast :: SomeString -> Maybe Duration
extractDurationFast inputBS = 
      let durStr = C.words (C.drop (timestampWidth + 1) inputBS) !! durationFNum
      in fmap fst $ C.readInt durStr 


type LineMatch = (SomeString -> Bool)

isCompleted :: LineMatch
isCompleted s = s =~ "Completed "::Bool --lazy bytestring char8 has no isInfixOf

-- TODO: this field number may vary if there is no severity in the log format
isProcessing :: LineMatch
isProcessing s = C.pack "Processing" ==  C.words (C.take 90 s) !! severityFNum

durationFNum = 5
severityFNum = 7
timestampWidth = 15
 
parseLogEvent :: SomeString -> Maybe LogEvent
parseLogEvent s = case extractCore s of
      Just (timestamp, _hostname, pid, firstWord) 
                | firstWord == C.pack "Processing" -> 
                    fmap (\a -> Start timestamp pid a) $ extractAction s
                | firstWord == C.pack "Completed" -> 
                    fmap (\d -> End timestamp pid d) $ extractDuration s
                | otherwise -> Nothing
      _ -> Nothing

