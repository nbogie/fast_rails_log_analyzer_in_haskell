#!/usr/bin/env gnuplot

##not master - copy in learning_gnuplot/pl_analyze
#set terminal wxt enhanced
#set terminal wxt font "FreeSans"
#set logscale x  # if we want to plot max

set xlabel "response time (ms)"
set ylabel "action"
set key top right box
set arrow 2 from 200,-1 to 200,20 nohead linetype 4

plot "< sort -k4 -rn report.plz | head -n 16" using 4:0:ytic(1) every 1 title "mean", \
     "" using (300+$4):($0):4 with labels title "", \
     "" using 6:0 title "min"
     #"" using 7:0 title "max"

set terminal png
#set terminal png font Loma
#set output "averages.png"
#replot

