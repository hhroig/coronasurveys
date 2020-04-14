#!/bin/bash

#for f in ../../../data/PlotData/*.csv; do
#    country=`basename $f|sed 's/-esti.*//g'`

#    for langdir in `ls -d locales/*/`; do
    #	language=`basename $langdir`
for language in en; do
	echo "doing  $language"
	LC_ALL=$language ./estimates_plot.py -c FR -f 50  -o 10 -cs 35 -cfr 20 -d 3
	LC_ALL=$language ./estimates_plot.py -c ES -f 50  -o 10 -cs 40 -cfr 20 -d 3
	LC_ALL=$language ./estimates_plot.py -c IT -f 50 -o 3 -cs 39 -cfr 10 -d 10 -cstof -5
	LC_ALL=$language ./estimates_plot.py -c PT -f 1 -o 3 -cs 30 -cfr 15 -d 10 
	LC_ALL=$language ./estimates_plot.py -c CY -f 10 -o 2 -cs 12 -cfr 10 -d 2 -cfrtof 6 -cstof 0
	LC_ALL=$language ./estimates_plot.py -c GB -f 60 -o 10 -cs 30 -cfr 20 -d 10 -cstof -4
	LC_ALL=$language ./estimates_plot.py -c US -f 60 -o 5 -cs 32 -cfr 15 -d 10 
	LC_ALL=$language ./estimates_plot.py -c UA -f 1  -o 5 -cs 20 -cfr 15 -d 3 -cfrtof 4

	
done

