module RLA.Types where

import qualified Data.ByteString.Lazy.Char8 as C

-- some protection against possible changes of string impl
type SomeString = C.ByteString

type Pid = Int

type Action = (SomeString, Maybe Format)
showAction (a, Just fmt) = C.unpack a ++ "."++C.unpack fmt
showAction (a, Nothing) = C.unpack a

type Format = SomeString -- format is json, csv, xml, etc.
type Duration = Int
type Timestamp = SomeString -- we currently don't need to parse these.
type Hostname = SomeString
type Severity = SomeString

-- in the log an action may start or end, on a given pid.
data LogEvent = Start Hostname Timestamp Pid Action
                | End Hostname Timestamp Pid Duration 

instance Show LogEvent where
  show (Start h t p a) = "<<Start at " ++ C.unpack t ++ ", pid: "++ show p ++ ", action: "++showAction a ++ " on " ++ show h ++ ">>"
  show (End h t p d) = "<<End at " ++ C.unpack t ++ ", pid: "++ show p ++ ", duration: "++show d ++ " on " ++ show h ++ ">>"

