#!/bin/bash

#for f in ../../../data/PlotData/*.csv; do
#    country=`basename $f|sed 's/-esti.*//g'`

#    for langdir in `ls -d locales/*/`; do
    #	language=`basename $langdir`
for language in en; do
	echo "doing  $language"
	LC_ALL=$language ./estimates_plot.py -c FR -f 50 -o 10 
	LC_ALL=$language ./estimates_plot.py -c ES -f 50 -x 8
	LC_ALL=$language ./estimates_plot.py -c IT -f 50 -x 12
	LC_ALL=$language ./estimates_plot.py -c PT -f 1 -d 10 -x 3
	LC_ALL=$language ./estimates_plot.py -c CY -f 10 -d 10 #-x 3
	LC_ALL=$language ./estimates_plot.py -c GB -f 60 -d 10  -x 5
	LC_ALL=$language ./estimates_plot.py -c US -f 60 -d 10 -x 5
	
done

