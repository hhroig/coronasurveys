#!/bin/bash

(
    echo 'answers,date'
    for f in ../../../data/*-*.csv
    do
        cut -d, -f1 "$f" | \
            sed -e 's/"//g' | \
            cut -d' ' -f1 | \
            tail -n+2;
    done | \
        sort | \
        uniq -c | \
        sed -e 's/^ *//' | \
        tr ' ' ',' | \
        head -n -1
) > overall.csv


for country in $(ls ../../../data/*-*.csv | cut -d'-' -f1 | sed -e 's@^.*/\([^/]*\)$@\1@' | sort -u)
do
    (
        echo 'answers,date'
        for f in ../../../data/$country-*.csv
        do
            cut -d, -f1 "$f" | \
                sed -e 's/"//g' | \
                cut -d' ' -f1 | \
                tail -n+2;
        done | \
            sort | \
            uniq -c | \
            sed -e 's/^ *//' | \
            tr ' ' ',' | \
            head -n -1
    ) > "overall_$country.csv"
done
