module Testing where

import Parser

import qualified Data.ByteString.Lazy.Char8 as C
gotest= do con <- testcontents 
           let l1 = takeFirstLine con
           return $ extractCoreWithRead l1

takeFirstLine con = head $ take 1 $ C.lines con
takeFirstFromFile = do con <- testcontents
                       return $ takeFirstLine con

testcontents = loadForTest "inputs/sensitive/100k_no_sev_201102_minimal.log"

loadForTest filename = C.readFile filename

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
