{-# LANGUAGE DeriveDataTypeable #-} --For CmdArgs
module Main where

import RLA.Analyzer

import qualified Data.ByteString.Lazy.Char8 as C
import System.Console.CmdArgs (cmdArgsMode, cmdArgsRun, (&=), summary, help)

-- provide reflection needed for cmdArgs
import Data.Typeable
import Data.Data
--
-- types for command-line args
data Prog = Prog 
            { outputFormat :: OutputFormat
            , mode :: ProgMode
            , outDir :: Maybe FilePath
            , threshold :: Int
            }
  deriving (Data, Typeable, Show)

data ProgMode = Analyze | Consolidate deriving (Data, Typeable, Show, Eq)

data OutputFormat = JSON | Plain deriving (Data, Typeable, Show, Eq)

-- set up cmd-line arg parsing, defaults, help
optsConfig = 
  cmdArgsMode $ Prog { mode = Analyze 
                       &= help "Program mode: Analyze or Consolidate"
                     , outputFormat = Plain 
                       &= help "Output format: JSON or Plain"
                     , outDir = Nothing &= help "Output dir for reports"
                     , threshold = 0 
                       &= help "Threshold ms (for consolidation filter)"
                     } 
  &= summary "Fast Rails Log Analyzer.  Parses from stdin."

main :: IO ()
main = do 
  opts <- cmdArgsRun optsConfig
  case mode opts of
    Analyze -> mainAnalyze opts
    Consolidate -> mainConsolidate opts

-- Analyze the rails log into stats, like pl_analyze
mainAnalyze ::  Prog -> IO ()
mainAnalyze opts = do
  content <- C.getContents
  let statsMap = makeStats content
  let present = if outputFormat opts == JSON 
                  then presentActionsAsJSON 
                  else presentActionsAsString
  case outDir opts of
    Nothing -> putStrLn $ present statsMap
    Just dir -> do
      writeReports statsMap dir
      putStrLn $ "Reports written into " ++ dir

-- consolidate the rails Processing/Completed line pairs into single line events
mainConsolidate :: Prog -> IO ()
mainConsolidate opts = do
  les <- fmap parseContents C.getContents
  let revs = consolidate les
  mapM_ print $ filter f revs
    where 
      f :: RailsEvent -> Bool
      f (RailsEvent _ac dur _p _st _et) = dur > thresh
      thresh = threshold opts
