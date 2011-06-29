{-# LANGUAGE OverloadedStrings #-} 
module RLA.Stats where

import RLA.Types

import Data.Aeson
--import qualified Data.Aeson.Types as T

import Text.Printf (printf)

data Stats = Stats 
  { minDur           :: !Duration -- ^ Minimum duration seen
  , maxDur           :: !Duration -- ^ Maximum duration seen
  , count            :: !Int      -- ^ Total number of occurrences
  , totalDurSquared  :: !Integer  -- ^ Sum of duration^2 of all occurrences 
                                  -- (This allows us to calculate stddev at end,
                                  --  without second pass)
  , totalDur :: !Int              -- ^ Sum of duration of all occurrences
 } deriving (Eq)

instance ToJSON Stats where
  toJSON s@(Stats mn mx c _tds td) = 
    object [ 
        "minDur"   .= mn
      , "maxDur"   .= mx
      , "count"    .= c
      , "average"  .= (round(calcAvg s)::Integer)
      , "totalDur" .= td
      ]

instance Show Stats where
  show s = "min: " ++ show (minDur s)
             ++ " max: " ++ show (maxDur s)
             ++ " tot: " ++ show (totalDur s)
             ++ " totDurSquared: " ++ show (totalDurSquared s)
             ++ " count: " ++ show (count s)
             ++ " avg: " ++ show (round (calcAvg s)::Integer)

calcAvg ::  Stats -> Float
calcAvg s = fromIntegral (totalDur s) / fromIntegral (count s)

calcStdDev ::  Stats -> Float
calcStdDev s = 
  sqrt ((fromIntegral (totalDurSquared s ) / fromIntegral (count s)) 
    - (mean*mean))
    where mean = calcAvg s

newStats :: Duration -> Stats
newStats d = 
  Stats { minDur = d 
        , maxDur = d 
        , count = 1
        , totalDur = d
        , totalDurSquared = fromIntegral (d * d)
        }

updateStats :: Stats -> Duration -> Stats
updateStats st d = 
  Stats { minDur = newMin
        , maxDur = newMax
        , count = count st + 1
        , totalDur = newTotalDur
        , totalDurSquared = newTotalDurSquared
        }
    where
      newMin =  minimum [minDur st,d]
      newMax =  maximum [maxDur st,d]
      newTotalDur =  totalDur st + d
      newTotalDurSquared =  totalDurSquared st + fromIntegral (d * d)

statsToS :: Stats -> String
statsToS s = let c = count s
                 mean = calcAvg s
                 -- stdev = sqrt((sum_x2 / n) - (mean * mean))
                 t  = totalDur s
                 sd = calcStdDev s
                 mn = minDur s
                 mx = maxDur s
             in printf "%10d %10.0f %10d %10.1f %10d %10d" 
                  c (mean::Float) t (sd::Float) mn mx
