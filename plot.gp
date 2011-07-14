#!/usr/bin/gnuplot
# add this to run this interactively -persist

#set autoscale
#set grid

set title "Rails request completion time"
set xlabel "Response time (ms)"
set ylabel "Number of Occurrences"
set boxwidth 50
set y2tics
#set term png
#set output "completion_times.png"
plot "histogram.dat" using 1:2 axes x1y1 w boxes t "count", \
"" u 1:3 axes x1y2 w l t "CDF"
