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
extractCore :: SomeString -> Maybe (Timestamp, Hostname, Pid, SomeString)
extractCore = extractCoreWithRead

-- "Feb 10 06:53:40 host1 rails[28275]: local3.info<158>: Processing ..."
--  ^--timestamp--^ ^host       ^pid^   ^-sev/priority/-^ ^--message---...   
extractCoreWithRead :: SomeString -> Maybe (Timestamp, Hostname, Pid, SomeString)
extractCoreWithRead input = 
  let timestamp = C.take timestampWidth input
      (hostname, pidPlus, firstWord) = 
        if logIncludesSeverity
          then let (host:pidPl:_severity:firstW:_rest) = wordsAfterTimestamp
               in (host, pidPl, firstW)
          else let (host:pidPl:firstW:_rest) = wordsAfterTimestamp 
               in (host, pidPl, firstW)
      wordsAfterTimestamp = C.words (C.drop (timestampWidth+1) input)
      -- yuck: TODO
      (pidStr, _afterPid) = C.break (==']') $ C.drop 1 $ snd $ C.break (=='[') pidPlus
  in case C.readInt pidStr of
       Just (pid, _) -> Just (C.copy timestamp, C.copy hostname, pid, firstWord)
       Nothing -> Nothing


-- Extract an action and an optional format.  May fail entirely: return Nothing.
-- The action will have its own copies of strings, and not pointers into 
-- the bytestring.
extractAction ::  SomeString -> Maybe Action
extractAction = extractActionFast

extractActionFast :: SomeString -> Maybe Action
extractActionFast inputBS = 
  case take 3 $ drop numFieldsBeforeAction fields of
    (action:w2:w3:[]) ->  Just (Action (C.copy action) (fmap C.copy format))
        where 
          format = if w2 == C.pack "to" then Just w3 else Nothing
    _ -> Nothing -- Unexpected line fmt
  where 
    fields = C.words $ C.drop (timestampWidth+1) inputBS

extractDuration ::  SomeString -> Maybe Duration
extractDuration = extractDurationFast

extractDurationFast :: SomeString -> Maybe Duration
extractDurationFast inputBS = 
      let durStr = C.words (C.drop (timestampWidth + 1) inputBS) !! durationFNum
      in fmap fst $ C.readInt durStr 

logIncludesSeverity ::  Bool
logIncludesSeverity = True
durationFNum ::  Int
durationFNum           =  if logIncludesSeverity then 5 else 4
numFieldsBeforeAction ::  Int
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
