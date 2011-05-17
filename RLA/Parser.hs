module RLA.Parser 
  (
    extractCore
  , extractAction
  , extractDuration
  , parseLogEvent
  ) where

import qualified Data.ByteString.Lazy.Char8 as C
-- import Data.List (isInfixOf)
-- import Text.ParserCombinators.Parsec

import RLA.Types

-- we may change this to extractCoreWithParsec, or other impl
extractCore = extractCoreWithRead

-- "Feb 10 06:53:40 host1 rails[28275]: local3.info<158>: Processing ..."
--  ^--timestamp--^ ^host       ^pid^   ^-sev/priority/-^ ^--message---...   
extractCoreWithRead :: SomeString -> Maybe (Timestamp, Hostname, Pid, SomeString)
extractCoreWithRead input = 
      let timestamp=C.take timestampWidth input
          (hostname, pidPlus, firstWord) = 
            if logIncludesSeverity
               then let (hostname:pidPlus:_severity:firstWord:_rest) = wordsAfterTimestamp in (hostname, pidPlus, firstWord)
               else let (hostname:pidPlus:firstWord:_rest) = wordsAfterTimestamp in (hostname, pidPlus, firstWord)
          wordsAfterTimestamp = C.words (C.drop (timestampWidth+1) input)
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
extractActionFast inputBS = let (action:w2:w3:[]) = take 3 (drop numFieldsBeforeAction (C.words (C.drop (timestampWidth+1) inputBS)))
                                format = if w2 == C.pack "to" then Just w3 else Nothing
                            in Just (C.copy action, fmap C.copy format)


extractDuration = extractDurationFast

extractDurationFast :: SomeString -> Maybe Duration
extractDurationFast inputBS = 
      let durStr = C.words (C.drop (timestampWidth + 1) inputBS) !! durationFNum
      in fmap fst $ C.readInt durStr 


logIncludesSeverity = False
durationFNum           =  if logIncludesSeverity then 5 else 4
numFieldsBeforeAction  =  if logIncludesSeverity then 4 else 3
timestampWidth = 15
 
parseLogEvent :: SomeString -> Maybe LogEvent
parseLogEvent s = case extractCore s of
      Just (timestamp, hostname, pid, firstWord) 
                | firstWord == C.pack "Processing" -> 
                    fmap (Start hostname timestamp pid) $ extractAction s
                | firstWord == C.pack "Completed" -> 
                    fmap (End hostname timestamp pid) $ extractDuration s
                | otherwise -> Nothing
      _ -> Nothing

