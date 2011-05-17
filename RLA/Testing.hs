module RLA.Testing where

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

runTestManyHosts = do
  c <- C.readFile fileNameForTest
  let stats = simplifyKeys $ makeStats c
  let failures = E.lefts $ map (\(ch,dur) -> ensureEntry ("NinjasController#action"++[ch], "csv") (assertEqual maxDur dur "dur") stats) $ zip "ABCD" [44, 22, 11, 33]
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



assertEqual fn expected label st = 
  if actual == expected 
    then Right $ show label ++ "has expected value " ++ show expected
    else Left $ "Wrong value for " ++ label ++ ". Expected "
                ++ show expected ++ " but got "++show actual 
                ++ ".  Record: "++show st
    where actual = fn st


example 0 = "Feb 10 06:53:40 host1 rails[28275]: local3.info<158>: Processing FooController#update to json (for 123.123.123.123 at 2011-02-10 06:53:40) [PUT] X-UniqueRequestId: 338304445520f73dd35499cf1351f2467ccc913c"
example 1 = "Feb 10 06:53:41 host1 rails[28275]: local3.info<158>: Completed in 18ms (View: 1, DB: 4) | 200 OK [http://example.com/foo/bar.json?_method=put]"
example 2 = "Feb 10 06:53:41 host1 rails[28275]: local3.info<158>: Processing BazController#update (for 123.123.123.123 at 2011-02-10 06:53:41) [PUT] X-UniqueRequestId: 6f98a508e66ce328effdb6be47330fa0857c57dd"
example 3 = "Feb 10 06:53:41 host1 rails[28275]: local3.info<158>: Completed in 27ms (View: 1, DB: 7) | 200 OK [http://example.com/baz/bar]"


-- this example has action and format
actionexample 0 = "Feb 10 06:53:40 host1 rails[28275]: local3.info<158>: Processing FooController#update to json (for 123.123.123.123 at 2011-02-10 06:53:40) [PUT] X-UniqueRequestId: 338304445520f73dd35499cf1351f2467ccc913c"
-- this example has no format
actionexample 1 = "Feb 10 06:53:41 host1 rails[28275]: local3.info<158>: Processing BazController#update (for 123.123.123.123 at 2011-02-10 06:53:41) [PUT] X-UniqueRequestId: 6f98a508e66ce328effdb6be47330fa0857c57dd"

test n = extractAction $ packedex n
packedex n = C.pack (actionexample n)
