## To compile a figure

To create a plot run the following, where `<CC>` is the country code:
```bash
LANG=<LANG> ./estimates_plot.py -c <CC>
LC_ALL=<LANG> ./estimates_plot.py -c <CC> -f 50
```


## TODO
* Make the arrow point to the right place
* Add labels to axes
* Add legend
* Make everything pretty
* Localize the dates
* Make this work To get the string translated in the language `<LANG>` and using worlwide data:
```bash
LANG=<LANG> ./estimates_plot.py

```

