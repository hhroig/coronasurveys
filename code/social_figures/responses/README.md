## To compile a figure

First, preprocess the data
```bash
./preprocess.sh
```

Then, to get the string translated in the language `<LANG>` and using worlwide data:
```bash
LANG=<LANG> ./overall_plot.py
```
or using a country's data whose code is `<CC>`:
```bash
LANG=<LANG> ./overall_plot.py -c <CC> overall_<CC>.csv
```


## TODO

* Localize the dates
