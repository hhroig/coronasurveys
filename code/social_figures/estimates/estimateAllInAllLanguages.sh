#!/bin/bash

#for f in ../../../data/PlotData/*.csv; do
#    country=`basename $f|sed 's/-esti.*//g'`

#    for langdir in `ls -d locales/*/`; do
    #	language=`basename $langdir`
for language in en; do
	echo "doing  $language"
	
	scale='t'
	    LC_ALL=$language ./estimates_plot.py -c FR -f 50  -o 10 -cs 35 -ccfr 20 -ccfrd 10 -cstof -5 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c ES -f 50  -o 10 -cs 40 -ccfr 20 -ccfrd 10 -cstof -5 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c IT -f 50 -o 3 -cs 39 -ccfr 10 -ccfrd 10 -cstof -5 --cosurerro --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c PT -f 1 -o 3 -cs 30 -ccfr 15 -ccfrd 10  --cosurerror  --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c CY -f 10 -o 2 -cs 15 -ccfr 5 -ccfrd 20 -ccfrtof 6 -cstof 0 -csd 15 -od 100 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c GB -f 60 -o 10 -cs 30 -ccfr 20 -ccfrd 10 -cstof -4 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c US -f 60 -o 5 -cs 32 -ccfr 15 -ccfrd 10 -cstof -5 -od 10 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c UA -f 1  -o 5 -cs 20 -ccfr 15 -ccfrd 10 -ccfrtof 4 -cstof -5 --cosurerror --logyscale $scale
	    scale='f'
	    	    LC_ALL=$language ./estimates_plot.py -c FR -f 50  -o 10 -cs 35 -ccfr 20 -ccfrd 10 -cstof -5 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c ES -f 50  -o 10 -cs 40 -ccfr 20 -ccfrd 10 -cstof -5 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c IT -f 50 -o 3 -cs 39 -ccfr 10 -ccfrd 10 -cstof -5 --cosurerro --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c PT -f 1 -o 3 -cs 30 -ccfr 15 -ccfrd 10  --cosurerror  --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c CY -f 10 -o 2 -cs 15 -ccfr 5 -ccfrd 20 -ccfrtof 6 -cstof 0 -csd 15 -od 100 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c GB -f 60 -o 10 -cs 30 -ccfr 20 -ccfrd 10 -cstof -4 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c US -f 60 -o 5 -cs 32 -ccfr 15 -ccfrd 10 -cstof -5 -od 10 --cosurerror --logyscale $scale
	    LC_ALL=$language ./estimates_plot.py -c UA -f 1  -o 5 -cs 20 -ccfr 15 -ccfrd 10 -ccfrtof 4 -cstof -5 --cosurerror --logyscale $scale

	
	done
done

