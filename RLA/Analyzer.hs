module RLA.Analyzer where

import Text.Printf (printf)
import Data.List (foldl', sortBy)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C

import RLA.Types
import RLA.Parser
import RLA.Stats (Stats, updateStats, newStats, statsToS, totalDur)
import Data.Aeson

-- a rails event reconstituted from a start and end LogEvent
data RailsEvent = 
  RailsEvent Action Duration Pid Timestamp Timestamp 
  deriving (Show)

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
      tally0 :: (PidMap, StatMap)
      tally0 = (M.empty, M.empty)
      logEvents = mapMaybe parseLogEvent
      (_tallyMap, statsMap) = foldl' tally tally0 $ logEvents ls
      ls = C.lines content

tally :: (PidMap, StatMap) -> LogEvent -> (PidMap, StatMap)

-- Note: We should record an error here if the map already has 
-- a start event recorded for this pid.
tally (pidmap, statMap) ev@(Start hostname _ pid _) = 
  (pidmap', statMap) 
    where pidmap' = M.insert (pid,hostname) ev pidmap

tally (pidmap, statMap) _ev@(End hostname _endTime pid duration) = 
  case M.lookup (pid, hostname) pidmap of
    Just (Start _hostname _startTime _ action) -> 
      case M.lookup action statMap of
        -- TODO: should be pidmap' - with this deleted.  More work but slightly 
        -- smaller footprint, depending on how many pids we get through
        Just st -> (pidmap, statMap')
          where 
            _pidmap' = M.delete (pid,hostname) pidmap
            -- The following strictness is critical for mem usage
            -- we want to insert the stat not a thunk of it
            statMap' = stat' `seq` M.insert action stat' statMap
            stat' = updateStats st duration
        Nothing -> (pidmap', statMap')
          where 
            pidmap' = M.delete (pid,hostname) pidmap
            statMap' = M.insert action (newStats duration) statMap
    -- if there's already an end for this pid, do nothing (lenient)
    Just (End _ _ _ _)                       -> (pidmap, statMap) 
    Nothing                                  -> (pidmap, statMap)


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
presentActionsAsJSON smap = do
  putStrLn $ C.unpack $ encode $ sortStats (M.assocs smap)

sortStats ::  [(a, Stats)] -> [(a, Stats)]
sortStats = reverse . sortBy (compare `on` totalDur . snd)
