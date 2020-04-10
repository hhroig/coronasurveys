#!/bin/bash

#for f in ../../../data/PlotData/*.csv; do
#    country=`basename $f|sed 's/-esti.*//g'`

#    for langdir in `ls -d locales/*/`; do
    #	language=`basename $langdir`
#for language in en; do

find locales -maxdepth 1 -mindepth 1 -type d | \
while read -r language
do
    language=$(basename "$language")
	echo "doing  $language"
    
	LC_ALL=$language ./overall_plot.py -o "responses_WW_lang_$language.jpg" -s -0.1
	LC_ALL=$language ./overall_plot.py -o "responses_FR_lang_$language.jpg" -s -0.15 -t 0.05 -c FR overall_FR.csv
	LC_ALL=$language ./overall_plot.py -o "responses_ES_lang_$language.jpg" -s -0.1 -c ES overall_ES.csv
	LC_ALL=$language ./overall_plot.py -o "responses_IT_lang_$language.jpg" -s -0.1 -t 0.1 -c IT overall_IT.csv
	LC_ALL=$language ./overall_plot.py -o "responses_PT_lang_$language.jpg" -s -0.1 -c PT overall_PT.csv
	LC_ALL=$language ./overall_plot.py -o "responses_CY_lang_$language.jpg" -s -0.2 -c CY overall_CY.csv
    #LC_ALL=$language ./overall_plot.py -o "responses_GB_lang_$language.jpg" -s  -c GB overall_GB.csv
	LC_ALL=$language ./overall_plot.py -o "responses_US_lang_$language.jpg" -s -0.1 -c US overall_US.csv
	
done

