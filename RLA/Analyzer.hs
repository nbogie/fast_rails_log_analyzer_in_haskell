module RLA.Analyzer where

import Text.Printf (printf)
import Data.List (foldl', sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C

import RLA.Types
import RLA.Parser
import RLA.Stats (Stats, updateStats, newStats, statsToS, totalDur)
import Data.Aeson

-- a rails event reconstituted from a start and end LogEvent
data RailsEvent = 
  RailsEvent Action Duration Pid Timestamp Timestamp 

instance Show RailsEvent where
  show (RailsEvent ac dur _pid _start stop) = 
    C.unpack stop ++ " " ++ show ac ++ " - " ++ " " ++ show dur 

type PidMap = M.Map (Pid,C.ByteString) LogEvent
type StatMap = M.Map Action Stats

simplifyKeys :: StatMap -> M.Map (String, Maybe String) Stats
simplifyKeys statMap = 
  M.mapKeys f statMap
    where f (Action ac fmtM) = (C.unpack ac, fmap C.unpack fmtM)


makeStats :: C.ByteString -> StatMap
makeStats content = 
  statsMap
    where 
      tally0 :: StatMap
      tally0 = M.empty
      logEvents = mapMaybe parseLogEvent
      railsEvents = consolidate . logEvents
      statsMap = foldl' tally tally0 $ railsEvents ls
      ls = C.lines content
  
tally :: M.Map Action Stats -> RailsEvent -> M.Map Action Stats
tally statMap _ev@(RailsEvent action duration _pid _start _stop) =
  case M.lookup action statMap of
    Just st -> statMap'
      where 
        -- The following strictness is critical for mem usage
        -- we want to insert the stat not a thunk of it
        statMap' = stat' `seq` M.insert action stat' statMap
        stat' = updateStats st duration
    Nothing -> statMap'
      where 
        statMap' = M.insert action (newStats duration) statMap

actionToS :: Action -> String
actionToS (Action name maybeFormat) = 
  let n = C.unpack name
      f = maybe "-" C.unpack maybeFormat
  in printf "%-50s %-10s" n f

presentActions :: StatMap -> IO ()
presentActions smap = 
  putStrLn $ unlines $ header:body
    where
      header = printf "%-50s %-10s %10s %10s %10s %10s %10s %10s" 
                     "Render Times Summary:" "Format" "Count" "Avg" 
                     "Total" "Std Dev" "Min" "Max" 
      body = map putIt $ sortStats (M.assocs smap)
      putIt (action, stats) = actionToS action ++ " " ++ statsToS stats

-- Note: Json will be missing stddev, 
-- as it just reflects Stats which doesn't carry it
presentActionsAsJSON :: StatMap -> IO ()
presentActionsAsJSON smap = 
  putStrLn $ C.unpack $ encode $ sortStats (M.assocs smap)

sortStats ::  [(a, Stats)] -> [(a, Stats)]
sortStats = reverse . sortBy (comparing (totalDur . snd))

parseContents ::  C.ByteString -> [LogEvent]
parseContents c = mapMaybe parseLogEvent $ C.lines c

consolidate :: [LogEvent] -> [RailsEvent]
consolidate levs = con levs M.empty
  where
    con :: [LogEvent] -> PidMap -> [RailsEvent]
    con [] _ = []

    con (ev@(Start hostname _ pid _):xs) pidmap = 
      con xs (M.insert (pid,hostname) ev pidmap)

    con (_ev@(End hostname endTime pid duration):xs) pidmap = 
      case M.lookup (pid, hostname) pidmap of
        Just (Start _hostname startTime _ action) -> 
          railsEv : rest
          -- railsEv `seq` (railsEv : rest)
            where
              railsEv = RailsEvent action duration pid startTime endTime 
              rest = con xs (M.delete (pid,hostname) pidmap)
        _other                                   -> con xs pidmap
