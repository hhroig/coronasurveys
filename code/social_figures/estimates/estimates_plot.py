#!/usr/local/bin/python3

import sys
import os
import argparse

import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as mticker
import matplotlib.patches as mpatches
import seaborn as sns

import gettext
import itertools
from matplotlib.text import OffsetFrom

## Arguments / Options


parser = argparse.ArgumentParser(description="Builds a social network friendly response status curve.")
parser.add_argument("-l", "--locale", help="the locale to choose the text from", default="en")
parser.add_argument("-c", "--country_code", help="the country code the data belongs to", default="WW")
parser.add_argument("-s", "--shift_contrib_x", type=float, help="how much to shift the arrow text on the x axis", default=0)
parser.add_argument("-f", "--first_datapoint", help="first datapoint to plot", type=int)
parser.add_argument("-o", "--official_arrow_datapoint", help="datapoint after the first plotted to point the arrow to for the official number", type=int, default=5)
#parser.add_argument("csv_filename", help="the csv file to process", default="../../../data/aggregate/FR-aggregate.csv", nargs="?")
args = parser.parse_args()

if args.first_datapoint:
    args.official_arrow_datapoint += args.first_datapoint
print(args.official_arrow_datapoint)

## Language

locale = gettext.translation('estimates_plot', localedir='locales')
locale.install()
_ = locale.gettext


## Preprocessing

filename="../../../data/PlotData/"+args.country_code+"-estimates.csv"
df = pd.read_csv(filename)
print (df['cum_cases'])
print (df['est_ccfr'])
print (df['estimated_cases'])

ccfrisgtzero=(df['est_ccfr'] > 0)

#df['date'] = pd.to_datetime(df['date'])
#df['answers'] = df['answers'].cumsum()

#print (df)

#exit
## Plot Settings

#figcolor=(40/255, 87/255, 128/255)
#figcolor=(27/255, 54/255, 93/255)
#figcolor=(28/255, 66/255, 32/255)
#figcolor=(63/255, 42/255, 86/255)
figcolor=(93/255, 62/255, 128/255)
sns.set(rc={'axes.facecolor': figcolor,
            'figure.facecolor': figcolor,
            'grid.color': 'black',
            'lines.linewidth': 3,
            'grid.linestyle': '--',
            'savefig.facecolor': figcolor,
            'axes.edgecolor': figcolor,
            'axes.labelcolor': 'white',
            'xtick.color': 'white',
            'ytick.color': 'white',
            'xtick.labelsize': 20,
            'ytick.labelsize': 20,
            'font.family': 'Gotham Rounded'})

fig, ax = plt.subplots(figsize=(10,10))
ax.set_yscale("log")
#palette = sns.color_palette(palette=sns.crayon_palette(sns.colors.crayons))
palette = sns.color_palette('colorblind') #sns.light_palette((210, 90, 60), input="husl"))
#sns.set_palette(palette)
new_palette = itertools.cycle(palette)
next(new_palette)

## Line Plots

## Official Estimate

next_color=palette[7]#next(new_palette)
snsplot = sns.lineplot(data=df, x='date', y='cum_cases', ax=ax, color=next_color)
official_arrow_x = df.loc[df.index[args.official_arrow_datapoint], 'date']
official_arrow_y = df.loc[df.index[args.official_arrow_datapoint], 'cum_cases']
official_arrow_text_x = df.loc[df.index[args.official_arrow_datapoint + 5], 'date']
ax.annotate(_('Confirmed cases'),
            xy=(official_arrow_x, official_arrow_y), xycoords='data',
            xytext=(official_arrow_text_x, official_arrow_y), textcoords='data',
            arrowprops=dict(arrowstyle='fancy', connectionstyle="arc3,rad=-0.2",
                            facecolor=next_color, edgecolor=next_color,
                            shrinkA=5, shrinkB=5,
                            relpos=(0, 0.5)),
            horizontalalignment='left', verticalalignment='center_baseline',
            fontfamily='Futura LT', fontsize=28, color=next_color)

## Estimate from Death Rate

next_color=next(new_palette)
sns.lineplot(data=df[ccfrisgtzero], x='date', y='est_ccfr', ax=ax, color=next_color)
death_estimate_arrow_x = df.loc[df.index[-20], 'date']
death_estimate_arrow_y = df.loc[df.index[-20], 'est_ccfr']
death_estimate_arrow_text_x = df.loc[df.index[-12], 'date']
ax.annotate(_('If 1.4% of the cases lead\nto death')+'$^*$',
            xy=(death_estimate_arrow_x, death_estimate_arrow_y), xycoords='data',
            xytext=(death_estimate_arrow_text_x, death_estimate_arrow_y / 1000), textcoords='data',
            arrowprops=dict(arrowstyle='fancy',connectionstyle="arc3,rad=0.4",
                            facecolor=next_color,edgecolor=next_color,
                            shrinkA=5,shrinkB=5,
                            relpos=(0.5, 1)),
            horizontalalignment='center', verticalalignment='top', multialignment='center',
            fontfamily='Futura LT', fontsize=28, color=next_color)

ccfrcolor=next_color
## CoronaSurveys Estimate
#sns.lineplot(data=df, x='date', y='estimated_cases', ax=ax, color=next(new_palette))

#next(new_palette)
next_color=next(new_palette)
sns.lineplot(data=df, x='date', y='estimated_cases', ax=ax, color=next_color) #, err_style="bars")
ix=0
while ix < len(df.index) and np.isnan(df.loc[df.index[ix], 'estimated_cases']):
    ix += 1
cosur_estimate_arrow_x = df.loc[df.index[ix], 'date']
cosur_estimate_arrow_y = df.loc[df.index[ix], 'estimated_cases']
cosur_estimate_arrow_text_x = df.loc[df.index[ix - 10], 'date']
cosur_estimate_arrow_text_y = df.loc[df.index[ix], 'estimated_cases']

ancosur=ax.annotate(_('CoronaSurveys'),
            xy=(cosur_estimate_arrow_x, cosur_estimate_arrow_y), xycoords='data',
            xytext=(cosur_estimate_arrow_text_x, cosur_estimate_arrow_y), textcoords='data',
            arrowprops=dict(arrowstyle='fancy',connectionstyle="arc3,rad=-0.4",
                            facecolor=next_color,edgecolor=next_color,
                            shrinkA=5,shrinkB=5,
                            relpos=(1, 1)),
            horizontalalignment='right', verticalalignment='top', multialignment='center',
            fontfamily='Futura LT', fontsize=28, fontweight='bold', color=next_color)


offset_from = OffsetFrom(ancosur, (0, -.1))
ax.annotate(_('help us get more data'),
            xy=(0, 0), xycoords=offset_from,
            horizontalalignment='left', verticalalignment='top', multialignment='center',
            fontfamily='Futura LT', fontsize=20, fontweight='normal', color=next_color)

#sns.lineplot(data=df, x='date', y='prop_cases', ax=ax, color=next(new_palette)) #, err_style="bars")

#sns.lineplot(data=df, x='date', y='dunbar_cases', ax=ax) #, err_style="bars")

## Date Limits
if args.first_datapoint:
    ax.set_xlim([df.loc[df.index[args.first_datapoint], 'date'], df.loc[df.index[-1], 'date']])

## Margins
plt.subplots_adjust(left=0.1, right=.9, top=0.9, bottom=0.25)


## Axes tikcks and tick labels
@mticker.FuncFormatter
def my_ytick_formatter(x, pos):
    tmp = np.log10(x)
    if tmp < 3:
        return str(int(x))
    elif tmp < 6:
        return str(int(x / 1000)) + 'k'
    else:
        return str(int(x / 1000000)) + 'M'

plt.xticks(rotation=20)
#plt.locator_params(axis='y', nbins=3)
ax.xaxis.set_major_locator(mdates.WeekdayLocator(byweekday=(mdates.MO))) #DayLocator(bymonthday=range(5,32,10)))
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %dth'))
ax.yaxis.set_major_formatter(my_ytick_formatter)

## Axes labels

xlabel=ax.set_xlabel(_('Want to help? Complete the survey at \n coronasurveys.org!'),
              labelpad=30, fontsize=28, fontstyle='italic', fontname='Futura LT')

offset_from_xlabel = OffsetFrom(xlabel, (0, 0))
ax.set_ylabel('')

#ax.annotate('$^*$'+_('estimation based on ')+'https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html',
#            xy=(280, -10), xycoords='offset_from_xlabel,
#            horizontalalignment='center', verticalalignment='top', multialignment='center',
#            fontfamily='Futura LT', fontsize=14, fontweight='normal', color=ccfrcolor)

ax.annotate('$^*$'+_('estimation based on ')+'https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html',
            xy=(280, -160), xycoords='axes points',
            horizontalalignment='center', verticalalignment='top', multialignment='center',
            fontfamily='Futura LT', fontsize=14, fontweight='normal', color=ccfrcolor)

## Title

if args.country_code == 'WW':
    title_subset = _('in the World')
elif args.country_code == 'AR':
    title_subset = _('in Argentina')
elif args.country_code == 'CL':
    title_subset = _('in Chile')
elif args.country_code == 'CY':
    title_subset = _('in Cyprus')
elif args.country_code == 'DE':
    title_subset = _('in Germany')
elif args.country_code == 'EC':
    title_subset = _('in Ecuador')
elif args.country_code == 'ES':
    title_subset = _('in Spain')
elif args.country_code == 'FR':
    title_subset = _('in France')
elif args.country_code == 'GB':
    title_subset = _('in the United Kingdom')
elif args.country_code == 'GR':
    title_subset = _('in Greece')
elif args.country_code == 'IT':
    title_subset = _('in Italy')
elif args.country_code == 'JP':
    title_subset = _('in Japan')
elif args.country_code == 'NL':
    title_subset = _('in the Netherlands')
elif args.country_code == 'PT':
    title_subset = _('in Portugal')
elif args.country_code == 'US':
    title_subset = _('in the USA')

ax.set_title(_('Covid-19 Cases Estimates ') + title_subset,
             color='white', size=29, fontweight='bold', fontname='Futura LT', pad=20)

## Arrow for Welcomed Contributions

#ax.annotate(_('You can contribute here\nwith your responses!'),
            #xy=(df.loc[df.index[-2], 'date'], df.loc[df.index[-10], 'estimated_cases'] / 2), xycoords='data',
            #xytext=(0.4 + args.shift_contrib_x, 0.9), textcoords='axes fraction',
            #arrowprops=dict(facecolor='white', connectionstyle="arc3,rad=-0.2"),
            #horizontalalignment='right', verticalalignment='center_baseline',
            #fontfamily='Futura LT', fontsize=28, color='white')

## Save the figure

#fig.savefig(os.path.splitext(filename)[0] + '.jpg', dpi=200)

#print os.path.split(filename)

fig.savefig(os.path.splitext(os.path.split(filename)[1])[0] +'-'+locale.info()['language']+ '.jpg', dpi=200)

