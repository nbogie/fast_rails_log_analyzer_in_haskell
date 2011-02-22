#!/bin/bash
set -e
set -u

input_file=inputs/sensitive/200M_with_sev_20110211.log
 < $input_file ./Main +RTS -s > /dev/null 2> s_report.txt
 < $input_file ./Main +RTS -hr -p
hp2ps -c -e8in Main.hp
evince Main.ps


