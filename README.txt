TODO: 
=====

json output

add summary stats (as original pl_analyze)

sort by count of action

No need for pid to be parsed as an Int - it's just used as a map index to match up a "Completed in" line with a "Processing" one.   Could remain a string, assuming that's not slow for map lookup.  However, as a String, it'd still need to be copied from the lazy bytestring, anyway.

