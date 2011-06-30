An experimental rails log processor similar to pl_analyze, implemented in 
haskell for speed and low memory footprint, as a learning exercise.

Yields average, min and max response times for each action, splitting each 
action also by requested format (json, xml, etc).

It is currently VERY fussy about the log format.

WARNING: This is the work of a haskell newbie and contains many examples of the
WRONG way to do things.
In particular, the log parsing is almost certainly better done with a standard 
parsing library from haskell (such as parsec) for flexibility and robustness. 
(I found parsec significantly slower but haven't yet investigated if that's my 
fault).

Nevertheless, if you are prepared to tweak the parser to your log format, it is 
fast enough to be useful over the common ruby tools.

Performance (anecdotal)
=======================
On my laptop it processes a 400Mb* log file in 8 seconds, using 6 Mb RAM.
1Gb test files cause the same footprint.

(*2.2 million lines total, 1.4 million relevant Processing/Completed lines, 
representing 700k requests)

USAGE:
======
# first, compile
cabal configure && cabal build && cabal install
# process (uncompressed) log file:
./rails-log-analyzer-hs < myhugerailslogfile.log > report
# process compressed log file: 
zcat myhugerailslogfile.log.gz | ./rails-log-analyzer-hs > report

# output to json instead of plain text (exact format liable to change)
./rails-log-analyzer-hs -o json < myhugerailslogfile.log > report

# Consolidate to simpler log file (experimental), also filtering on req duration
./rails-log-analyzer-hs --mode=Consolidate --threshold=2000 < myhugerailslogfile.log > worst_events.log 

TODO: 
=====
 * Finish json output (better json structure, add stddev)

 * Handle input reliably from multiple hosts
Currently, only pids are tracked, and this will work reliably on a log file 
aggregated from multiple hosts until two hosts happen to use the same pid.

 * Add summary stats (as original pl_analyze)

 * No need for pid to be parsed as an Int - it's just used as a map index to match 
up a "Completed in" line with a "Processing" one.   Could remain a string, 
assuming that's not slow for map lookup.  However, as a String, it'd still need 
to be copied from the lazy bytestring, anyway.

 * Consider parsec again, in order to make the parser more robust and flexible.

 * Allow collection of information about unparseable lines.

 * Parallelize.  It's difficult to find suitable split points in the log file.
There must be no request in progress at the split point, unless we want to have 
to reconcile unmatched pairs at the reduce stage.

 * Make into a cabal package (not necessarily publishing to hackage, though)
