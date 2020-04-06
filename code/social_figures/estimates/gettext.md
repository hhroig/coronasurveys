## Initialize / Update template

```bash
xgettext --package-name=coronasurveys-social-plot --package-version=1 -d estimates_plot -o locales/estimates_plot.pot estimates_plot.py
sed -i 's/CHARSET/UTF-8/' locales/estimates_plot.pot
```

## Initialize translation file

```bash
cd locales
mkdir -p <LANG>/LC_MESSAGES
msginit --no-translator --locale <LANG> --output-file <LANG>/LC_MESSAGES/estimates_plot.po --input estimates_plot.pot
```

## Update translation file

```bash
cd locales/<LANG>/LC_MESSAGES
msgmerge -U estimates_plot.po ../../estimates_plot.pot
```
or all at once:
```bash
cd locales
find . -maxdepth 2 -mindepth 2 -type d -exec msgmerge -U {}/estimates_plot.po estimates_plot.pot \;
```

## Compile the translation files

```bash
cd locales
find . -maxdepth 2 -mindepth 2 -type d -exec msgfmt --check --verbose --output-file {}/estimates_plot.mo {}/estimates_plot.po \;
```

## Update all

The script below runs all update steps

```bash
./updateAllMsgs.sh
```
