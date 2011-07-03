module RLA.Testing where
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

main = runTestTT tests

tests = TestList[ testParseGoodLines
                , testParseWithActionAndFormat
                , testParseWithActionNoFormat
                , testParseBadLines
                ]

testParseGoodLines = "parse good lines" ~:
  TestList $ map (\(s, exp) -> 
                   TestCase $ assertEqual ("testing "++s) (res s) exp
    ) goodExamples
    where res s = parseLogEvent $ C.pack s

-- These examples test a range of things, quickly.
-- Has action and format
goodExample 0 = ( "Dec  1 00:00:00 appserver3 rails[999]: local3.info<158>: Processing FooController#someaction to json (for 123.123.123.123 at 2011-02-10 06:53:40) [PUT] X-UniqueRequestId: 338304445520f73dd35499cf1351f2467ccc913c"
                , Just $ mkStart "appserver3" "Dec  1 00:00:00" 999 $ mkActionFmt "FooController#someaction" "json" )

goodExample 1 = ( "Feb 10 06:53:40 host1 otherprogname[123]: local3.info<158>: Completed in 18ms (View: 1, DB: 4) | 200 OK [http://example.com/foo/bar.json?_method=put]"
                , Just $ mkEnd "host1" "Feb 10 06:53:40" 123 18)

-- Has action no format
goodExample 2 = ( "Feb 10 06:53:40 host1 rails[28275]: local3.info<158>: Processing BazController#update (for 123.123.123.123 at 2011-02-10 06:53:41) [PUT] X-UniqueRequestId: 6f98a508e66ce328effdb6be47330fa0857c57dd"
                , Just $ mkStart "host1" "Feb 10 06:53:40" 28275 $ mkActionNoFmt "BazController#update")

goodExample 3 = ( "Feb  3 23:59:58 h2 rails[2]: local3.info<158>: Completed in 27ms (View: 1, DB: 7) | 200 OK [http://example.com/baz/bar]"
                , Just $ mkEnd "h2" "Feb  3 23:59:58" 2 27)

goodExamples = map goodExample [0..3]

testParseWithActionAndFormat = 
  TestCase $ assertEqual "Parse action and format" expected res
    where 
      res = extractAction $ C.pack inp
      expected = Just $ mkActionFmt "FooController#update" "json"
      inp = "Feb 10 06:53:40 host1 rails[28275]: local3.info<158>: Processing FooController#update to json (for 123.123.123.123 at 2011-02-10 06:53:40) [PUT] X-UniqueRequestId: 338304445520f73dd35499cf1351f2467ccc913c"

testParseWithActionNoFormat = 
  TestCase $ assertEqual "Parse action, no format" expected res
    where 
      res = extractAction $ C.pack inp
      expected = Just $ mkActionNoFmt "BazController#update"
      inp = "Feb 10 06:53:41 host1 rails[28275]: local3.info<158>: Processing BazController#update (for 123.123.123.123 at 2011-02-10 06:53:41) [PUT] X-UniqueRequestId: 6f98a508e66ce328effdb6be47330fa0857c57dd"

testParseBadLines = 
  "Parse bad lines" ~: 
    TestList  [ testParseBadLineEmpty
              , testParseBadLineEmptyAfterTimestamp
              ]

testParseBadLineEmpty = 
  "empty" ~: TestCase $ assertEqual "Parse bad line" Nothing res
    where res = parseLogEvent $ C.pack ""

testParseBadLineEmptyAfterTimestamp = 
  "empty after... " ~: 
    TestList [ "timestamp" ~: Nothing ~?= try "Feb 10 06:53:41"
             , f "Feb 10 06:53:41 host1 rails[28275]: local3.info<158>: P"
             , f "Feb 10 06:53:41 host1 rails[28275]: "
             , f "Feb 10 06:53:41 host1 rails[28275]:"
             , f "Feb 10 06:53:41 host1 rails[28275]"
             ]
    where try inpStr = parseLogEvent $ C.pack inpStr
          f inpStr = ("Text: " ++ inpStr) ~: Nothing ~?= try inpStr

-- helpers to make actions and log events for expected results in testing, 
-- without needing to call C.pack everywhere.
-- Can the string language extension pack strings automatically for us?
mkActionFmt :: String -> String -> Action
mkActionFmt a fmt = Action (C.pack a) (Just (C.pack fmt))
mkActionNoFmt :: String -> Action
mkActionNoFmt a = Action (C.pack a) Nothing
mkEnd :: String -> String -> Int -> Int -> LogEvent
mkEnd h t pid d = let p = C.pack in End (p h) (p t) pid d
mkStart :: String -> String -> Int -> Action -> LogEvent
mkStart h t pid a = let p = C.pack in Start (p h) (p t) pid a
