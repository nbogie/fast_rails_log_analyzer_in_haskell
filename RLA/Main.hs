{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import RLA.Analyzer

import qualified Data.ByteString.Lazy.Char8 as C
import System.Console.CmdArgs (cmdArgsMode, cmdArgsRun, (&=), summary, help)

-- provide reflection needed for cmdArgs
import Data.Typeable
import Data.Data
--
-- types for command-line args
data Prog = Prog { outputFormat :: OutputFormat } 
  deriving (Data, Typeable, Show)

data OutputFormat = JSON | Plain 
  deriving (Data, Typeable, Show, Eq)


-- set up cmd-line arg parsing, defaults, help
optsConfig = 
  cmdArgsMode $ Prog { outputFormat = Plain 
                        &= help "Output format: JSON or Plain" } 
  &= summary "Fast Rails Log Analyzer.  Parses from stdin."

main :: IO ()
main = mainNormal

mainNormal ::  IO ()
mainNormal = do 
  opts <- cmdArgsRun optsConfig
  content <- C.getContents
  let statsMap = makeStats content
  let present = if outputFormat opts==JSON 
                  then presentActionsAsJSON 
                  else presentActions
  present statsMap
  return ()

mainNew :: IO ()
mainNew  = do
  les <- fmap parseContents C.getContents
  let revs = consolidate les
  mapM_ print $ filter f revs
    where f :: RailsEvent -> Bool
          f (RailsEvent _ac dur _p _st _et) = dur > 1000
