{-# LANGUAGE OverloadedStrings #-} 
module RLA.Types where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson

-- some protection against possible changes of string impl
type SomeString = C.ByteString

type Pid = Int

data Action = Action SomeString (Maybe Format) 
  deriving (Ord, Eq)

instance Show Action where
  show (Action ac fmtMaybe) = C.unpack ac ++ " " ++ maybe "-" C.unpack fmtMaybe

instance ToJSON Action where
  toJSON (Action name maybeFmt) = 
    object [ 
        "action" .= name
      , "format" .= maybeFmt
      ]

showAction ::  Action -> String
showAction (Action a (Just fmt)) = C.unpack a ++ "." ++ C.unpack fmt
showAction (Action a Nothing) = C.unpack a

type Format = SomeString -- format is json, csv, xml, etc.
type Duration = Int
type Timestamp = SomeString -- we currently don't need to parse these.
type Hostname = SomeString
type Severity = SomeString

-- in the log an action may start or end, on a given pid.
data LogEvent = Start Hostname Timestamp Pid Action
              | End Hostname Timestamp Pid Duration 
  deriving (Eq)

instance Show LogEvent where
  show (Start h t p a) =
    "<<Start at " ++ C.unpack t ++ ", pid: " ++ show p 
    ++ ", action: " ++ showAction a ++ " on " ++ show h ++ ">>"
  show (End h t p d) = 
    "<<End at " ++ C.unpack t ++ ", pid: " ++ show p 
    ++ ", duration: " ++ show d ++ " on " ++ show h ++ ">>"
