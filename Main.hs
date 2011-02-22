module Main where

import Text.Printf (printf)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C

import Types
import Parser
import Stats (Stats, updateStats, newStats, statsToS)

-- a rails event reconstituted from a start and end LogEvent
data RailsEvent = RailsEvent Action Duration Pid Timestamp Timestamp deriving (Show)

type PidMap = M.Map Pid LogEvent
type StatMap = M.Map Action Stats

main :: IO ()
main = do content <- C.getContents
          let ls = C.lines content
          let (_tallyMap, statsMap) = foldl' tally tally0 $ logEvents ls
          presentActions statsMap
          return ()
            where tally0 :: (PidMap, StatMap)
                  tally0 = (M.empty, M.empty)
                  logEvents = mapMaybe parseLogEvent

tally :: (PidMap, StatMap) -> LogEvent -> (PidMap, StatMap)

-- Note: We should record an error here if the map already has 
-- a start event recorded for this pid.
tally (pidmap, statMap) ev@(Start _ pid _) = (pidmap', statMap) 
                                 where pidmap' = M.insert pid ev pidmap

tally (pidmap, statMap) ev@(End endTime pid duration) = case M.lookup pid pidmap of
       Just (Start startTime _ action) -> 
            case M.lookup action statMap of
              Just st -> (pidmap, statMap')
                where 
                  pidmap' = M.delete pid pidmap
                  -- The following strictness is critical for mem usage
                  -- we want to insert the stat not a thunk of it
                  statMap' = stat' `seq` M.insert action stat' statMap
                  stat' = updateStats st duration
              Nothing -> (pidmap', statMap')
                where 
                  pidmap' = M.delete pid pidmap
                  statMap' = M.insert action (newStats duration) statMap
       Just (End _ _ _)                -> (pidmap, statMap) -- if there's already an end for this pid, do nothing (lenient)
       Nothing                         -> (pidmap, statMap)


actionToS :: Action -> String
actionToS action@(name,maybeFormat) = let n = C.unpack name
                                          f= maybe "-" C.unpack maybeFormat
                                      in printf "%-50s %-10s" n f


presentActions :: StatMap -> IO ()
presentActions smap = putStrLn $ unlines $ header:body
            where
               header = printf "%-50s %-10s %10s %10s %10s %10s %10s" "Render Times Summary:" "Format" "Count" "Avg" "Std Dev" "Min" "Max" 
               body = map putIt (M.assocs smap)
               putIt (action, stats) = actionToS action ++ " " ++ statsToS stats
