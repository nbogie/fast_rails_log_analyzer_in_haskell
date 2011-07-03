module RLA.DemoHelp where
-- Currently, these functions just exist to help manual testing via ghci

import Test.HUnit

import RLA.Parser
import RLA.Types
import RLA.Utils
import RLA.Analyzer
import RLA.Stats

import Data.Maybe (mapMaybe)
import qualified Data.Either as E
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M

gotest = 
    do ls <- linesOfFile fileNameForTest
       let l1 = head ls
       return $ extractCore l1

fileNameForTest = "inputs/test_many_hosts.log"

demoConsolidation = do
  c <- C.readFile fileNameForTest
  let les = mapMaybe parseLogEvent $ C.lines c
  let revs = consolidate les
  print revs
  return revs

runTestManyHosts = do
  c <- C.readFile fileNameForTest
  let stats = simplifyKeys $ makeStats c
  let failures = E.lefts $ map f $ zip "ABCD" [44, 22, 11, 33]
        where f (ch,dur) = ensureEntry ("NinjasController#action"++[ch], "csv")
                            (myAssertEqual maxDur dur "dur") stats
  if null failures
    then putStrLn "All tests passed"
    else putStrLn $ "FAILURES: \n" ++ unlines failures ++ "\nEND FAILURES"
  return stats

ensureEntry (ac, fmt) checkFn stats = 
    case M.lookup k stats of
      Just x -> 
        case checkFn x of
          Left e -> Left $ "Bad record for "++show k ++ ": "++ e
          Right e -> Right $ "entry exists ok with key "++ show k
      Nothing -> Left $ "No entry found for key " ++ show k 
                         ++ ". Map has keys: "++ show (M.keys stats)
      where k = (ac, Just fmt)

myAssertEqual fn expected label st = 
  if actual == expected 
    then Right $ show label ++ "has expected value " ++ show expected
    else Left $ "Wrong value for " ++ label ++ ". Expected "
                ++ show expected ++ " but got "++show actual 
                ++ ".  Record: "++show st
    where actual = fn st

