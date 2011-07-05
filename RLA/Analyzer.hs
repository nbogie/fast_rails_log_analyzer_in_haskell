module RLA.Analyzer where

import Text.Printf (printf)
import Data.List (foldl', sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C

import RLA.Types
import RLA.Parser
import RLA.Stats (Stats, updateStats, newStats, statsToS, totalDur, count, minDur, maxDur)
import Data.Aeson
import System.Directory (createDirectoryIfMissing)

type PidMap = M.Map (Pid,Hostname) LogEvent
type StatMap = M.Map Action Stats

simplifyKeys :: StatMap -> M.Map (String, Maybe String) Stats
simplifyKeys statMap = 
  M.mapKeys f statMap
    where f (Action ac fmtM) = (C.unpack ac, fmap C.unpack fmtM)

makeStats :: C.ByteString -> StatMap
makeStats content = 
  foldl' tally M.empty $ makeRailsEvents $ C.lines content
    where 
      logEvents = mapMaybe parseLogEvent
      makeRailsEvents = consolidate . logEvents
  
tally :: M.Map Action Stats -> RailsEvent -> M.Map Action Stats
tally statMap ev@(RailsEvent action duration _pid _start _stop) =
  case M.lookup action statMap of
    Just st -> statMap'
      where 
        -- The following strictness is critical for mem usage
        -- we want to insert the stat not a thunk of it
        statMap' = stat' `seq` M.insert action stat' statMap
        stat' = updateStats st ev
    Nothing -> statMap'
      where 
        statMap' = M.insert action (newStats duration) statMap

actionToS :: Action -> String
actionToS (Action name maybeFormat) = 
  let n = C.unpack name
      f = maybe "-" C.unpack maybeFormat
  in printf "%-50s %-10s" n f

writeReports :: StatMap -> FilePath -> IO ()
writeReports statMap dir = do
  createDirectoryIfMissing True dir
  writeFile (dir ++ "/report.json") (presentActionsAsJSON statMap)
  mapM_ repToFile [ (totalDur, "totalDuration")
                  , (minDur, "minTime")
                  , (maxDur, "maxTime")
                  , (count, "numRequests") ]
    where 
      repToFile (sf, n) = 
        writeFile fname $ presentActionsBy sf statMap
          where fname = dir ++ "/" ++ n ++ ".txt"

presentActionsAsJSON :: StatMap -> String
presentActionsAsJSON smap = 
  C.unpack $ encode $ sortStats totalDur (M.assocs smap)

presentActionsAsString :: StatMap -> String
presentActionsAsString smap = presentActionsBy totalDur smap

presentActionsBy :: (Ord c) => (Stats -> c) -> M.Map Action Stats -> String
presentActionsBy f smap = 
  unlines $ header:body
    where
      header = printf "%-50s %-10s %10s %10s %10s %10s %10s %10s" 
                     "Render Times Summary:" "Format" "Count" "Avg" 
                     "Total" "Std Dev" "Min" "Max" 
      body = map putIt $ sortStats f (M.assocs smap)
      putIt (action, stats) = actionToS action ++ " " ++ statsToS stats

-- sortStats ::  [(a, Stats)] -> [(a, Stats)]
sortStats ::  (Ord c) => (b -> c) -> [(a, b)] -> [(a, b)]
sortStats f = reverse . sortBy (comparing (f . snd))

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
