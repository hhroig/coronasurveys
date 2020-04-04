## Initialize / Update template

```bash
xgettext --package-name=coronasurveys-social-plot --package-version=1 -d overall_plot -o locales/overall_plot.pot overall_plot.py
sed -i 's/CHARSET/UTF-8/' locales/overall_plot.pot
```

## Initialize translation file

```bash
cd locales
mkdir -p <LANG>/LC_MESSAGES
msginit --no-translator --locale <LANG> --output-file <LANG>/LC_MESSAGES/overall_plot.po --input overall_plot.pot
```

## Update translation file

```bash
cd locales/<LANG>/LC_MESSAGES
msgmerge -U overall_plot.po ../../overall_plot.pot
```

## Compile the translation files

```bash
cd locales
find . -maxdepth 2 -mindepth 2 -type d -exec echo msgfmt --check --verbose --output-file {}/overall_plot.mo {}/overall_plot.po \;
```
